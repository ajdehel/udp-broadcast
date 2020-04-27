#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>


extern "C" {


/*******************************************************************************
 *  get_socket
 *  params:
 *    None
 *  returns:
 *    a socket file descriptor (a positive int)
 ******************************************************************************/
int get_socket( void );

/*******************************************************************************
 *  get_socket
 *  params:
 *    None
 *  returns:
 *    a socket file descriptor (a positive int)
 ******************************************************************************/
int get_broadcast_socket( void );


/*******************************************************************************
 *  bind_socket
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Address
 *    ip_port: short    => Port Number
 *  returns:
 *    0 on success
 ******************************************************************************/
int bind_socket( int *sockfd, char *ip_host, short ip_port );


/*******************************************************************************
 *  send_msg
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Address to sent the message to
 *    ip_port: short    => Port to send the message to
 *    msg:     char*    => The message
 *    msg_len: unsigned => The length of the message
 *  returns:
 *    The number of bytes sent
 ******************************************************************************/
int send_msg( int sockfd,
              char *ip_host,
              short ip_port,
              char *msg,
              unsigned msg_len );

/*******************************************************************************
 *  broadcast_msg
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Broadcast Address to send the message to
 *    ip_port: short    => Port to broadcast the message on
 *    msg:     char*    => The message
 *    msg_len: unsigned => The length of the message
 *  returns:
 *    The number of bytes sent
 ******************************************************************************/
int broadcast_message( int sockfd,
                       char *ip_host,
                       short ip_port,
                       char *msg,
                       unsigned msg_len );


/*******************************************************************************
 *  recv_msg
 *  params:
 *    sockfd:  int             => socket file descriptor
 *    msg:     char*           => A buffer to store the received message
 *    msg_len: unsigned*       => The length of the received message
 *    ip_host: char*           => IPv4 Broadcast Address that sent the message
 *    ip_port: unsigned short* => Port the message was sent on
 *  returns:
 *    The number of bytes received
 ******************************************************************************/
int recv_msg( int sockfd,
              char *msg,
              unsigned *msg_len,
              char *ip_host,
              unsigned short *ip_port );


}
