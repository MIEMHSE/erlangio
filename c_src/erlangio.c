#if defined(CONFIG_MODVERSIONS) && ! defined(MODVERSIONS)
    #include <linux/modversions.h>
    #define MODVERSIONS
#endif
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/miscdevice.h>
#include <linux/fs.h>
#include <asm/uaccess.h>
#include <asm/errno.h>

#define DRIVER_NAME "erlangio"
#define DRIVER_AUTHOR "Sergey Sobko <S.Sobko@profitware.ru>"
#define DRIVER_DESC "Erlang IO virtual device."

#include "erlangio.h"


MODULE_LICENSE( "Dual MIT/GPL" );
MODULE_AUTHOR( DRIVER_AUTHOR );
MODULE_DESCRIPTION( DRIVER_DESC );
MODULE_SUPPORTED_DEVICE( DRIVER_NAME );

static int minor = 0;
module_param( minor, int, S_IRUGO );

static int device_opened = 0;  /* Is device open?  Used to prevent multiple
                                        access to the device */
static char msg[BUF_LEN];    /* The msg the device will give when asked    */
static char *msg_ptr;

static struct file_operations misc_fops = {
  .owner  = THIS_MODULE,
  .read = device_read,
  .write = device_write,
  .open = device_open,
  .release = device_release
};

static struct miscdevice misc_dev = {
   .minor = MISC_DYNAMIC_MINOR,    // автоматически выбираемое
   .name = DRIVER_NAME,
   .fops = &misc_fops,
   .mode = S_IRUGO
};

/* Functions */

static int __init md_init( void )
{
    int ret;
    if( minor != 0 )
    {
        misc_dev.minor = minor;
    }
    ret = misc_register( &misc_dev );
    if( ret )
    {
        printk( KERN_ERR "%% Unable to register misc device\n" );
    }
    return ret;
}


static void __exit md_exit( void )
{
    misc_deregister( &misc_dev );
}


/* Methods */

static int device_open( struct inode *inode, struct file *file )
{
    static int counter = 0;
    if ( device_opened )
    {
        return -EBUSY;
    }

    device_opened++;
    sprintf( msg, "Test message. Device was accessed %d time(s).\n", counter++ );
    msg_ptr = msg;

    return SUCCESS;
}

static int device_release(struct inode *inode, struct file *file)
{
    device_opened --;

    return 0;
}

static ssize_t device_read(struct file *filp, char *buffer, size_t length, loff_t *offset)
{
    int bytes_read = 0;

    if ( *msg_ptr == 0 )
    {
        return 0;
    }

    while ( length && *msg_ptr )
    {
        put_user( *( msg_ptr++ ), buffer++ );

        length--;
        bytes_read++;
    }

    return bytes_read;
}


static ssize_t device_write( struct file *filp, const char *buff, size_t len, loff_t *off )
{
    printk ( KERN_ERR "%% Sorry, this operation isn't supported.\n" );
    return -EINVAL;
}

module_init( md_init );
module_exit( md_exit );