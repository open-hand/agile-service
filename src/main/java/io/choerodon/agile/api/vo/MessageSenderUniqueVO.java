package io.choerodon.agile.api.vo;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午4:51
 */
public class MessageSenderUniqueVO {
    
    public MessageSenderUniqueVO(MessageSender messageSender){
        this.msgCode = messageSender.getMessageCode();
        this.receiverList = Optional.ofNullable(messageSender.getReceiverAddressList()).map(HashSet::new).orElse(new HashSet<>());
        this.ccList = Optional.ofNullable(messageSender.getCcList()).map(HashSet::new).orElse(new HashSet<>());
        this.tenantId = messageSender.getTenantId();
        this.noticeTypeList = Optional.ofNullable(messageSender.getTypeCodeList()).map(HashSet::new).orElse(new HashSet<>());
    }
    
    public MessageSenderUniqueVO(){
    }

    /**
     * 租户Id
     */
    @ApiModelProperty(value = "租户Id")
    private Long tenantId;
    /**
     * 消息代码
     */
    @ApiModelProperty(value = "消息代码")
    private String msgCode;
    /**
     * 通知类型
     */
    @ApiModelProperty(value = "通知类型")
    private Set<String> noticeTypeList;
    
    /**
     * 接收人
     */
    @ApiModelProperty(value = "接收人")
    private Set<Receiver> receiverList;
    /**
     * 抄送人
     */
    @ApiModelProperty(value = "抄送人")
    private Set<String> ccList;


    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public String getMsgCode() {
        return msgCode;
    }

    public void setMsgCode(String msgCode) {
        this.msgCode = msgCode;
    }

    public Set<String> getNoticeTypeList() {
        return noticeTypeList;
    }

    public void setNoticeTypeList(Set<String> noticeTypeList) {
        this.noticeTypeList = noticeTypeList;
    }

    public Set<Receiver> getReceiverList() {
        return receiverList;
    }

    public void setReceiverList(Set<Receiver> receiverList) {
        this.receiverList = receiverList;
    }

    public Set<String> getCcList() {
        return ccList;
    }

    public void setCcList(Set<String> ccList) {
        this.ccList = ccList;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("{");
        sb.append("\"tenantId\":")
                .append(tenantId);
        sb.append(",\"msgCode\":\"")
                .append(msgCode).append('\"');
        sb.append(",\"noticeTypeList\":")
                .append(noticeTypeList);
        sb.append(",\"receiverList\":")
                .append(receiverList);
        sb.append(",\"ccList\":")
                .append(ccList);
        sb.append('}');
        return sb.toString();
    }
}
