#if defined(CONFIG_MODVERSIONS) && ! defined(MODVERSIONS)
    #include <linux/modversions.h>
    #define MODVERSIONS
#endif
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/miscdevice.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>
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

static int device_opened = 0;  // Times device is opened
static char msg[BUF_LEN];  // Message buffer
static char *msg_ptr;

static struct file_operations misc_fops = {
  .owner  = THIS_MODULE,
  .read = device_read,
  .write = device_write,
  .open = device_open,
  .release = device_release
};

static struct miscdevice misc_dev = {
   .minor = MISC_DYNAMIC_MINOR,  // auto
   .name = DRIVER_NAME,
   .fops = &misc_fops,
   .mode = S_IRUGO | S_IWUSR
};

struct proc_dir_entry *proc_file_entry;

static const struct file_operations proc_file_fops = {
    .owner = THIS_MODULE,
    .open  = device_open,
    .read  = device_read,
    .release = device_release
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

    proc_file_entry = proc_create("erlangio", 0, NULL, &proc_file_fops);

    if(proc_file_entry == NULL)
        return -ENOMEM;

    memset( msg, 0, BUF_LEN );
    msg_ptr = msg;

    return ret;
}


static void __exit md_exit( void )
{
    remove_proc_entry( "erlangio", NULL );
    misc_deregister( &misc_dev );
}


/* Methods */

static int device_open( struct inode *inode, struct file *file )
{
    if ( device_opened )
    {
        return -EBUSY;
    }

    device_opened++;

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


static ssize_t device_write( struct file *filp, const char *buffer, size_t length, loff_t *offset )
{
	int bytes_read = 0;

	for ( bytes_read = 0; bytes_read < length && bytes_read < BUF_LEN; bytes_read++ )
		get_user( msg[bytes_read], buffer + bytes_read );

    msg[bytes_read] = '\0';
    msg_ptr = msg;

	return bytes_read;
}

module_init( md_init );
module_exit( md_exit );