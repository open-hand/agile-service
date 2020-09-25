package io.choerodon.agile.api.vo;

import java.util.List;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午4:51
 */
public class MessageSenderUniqueVO {
    
    public MessageSenderUniqueVO(MessageSender messageSender){
        this.msgCode = messageSender.getMessageCode();
        this.receiverList = messageSender.getReceiverAddressList();
        this.ccList = messageSender.getCcList();
        this.tenantId = messageSender.getTenantId();
        this.noticeTypeList = messageSender.getTypeCodeList();
    }
    
    public MessageSenderUniqueVO(){
    }

    /**
     * 租户Id
     */
    private Long tenantId;
    /**
     * 消息代码
     */
    private String msgCode;
    /**
     * 接收人
     */
    private List<Receiver> receiverList;
    /**
     * 抄送人
     */
    private List<String> ccList;
    /**
     * 通知类型
     */
    private List<String> noticeTypeList;

    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public List<String> getCcList() {
        return ccList;
    }

    public void setCcList(List<String> ccList) {
        this.ccList = ccList;
    }

    public List<String> getNoticeTypeList() {
        return noticeTypeList;
    }

    public void setNoticeTypeList(List<String> noticeTypeList) {
        this.noticeTypeList = noticeTypeList;
    }

    public String getMsgCode() {
        return msgCode;
    }

    public void setMsgCode(String msgCode) {
        this.msgCode = msgCode;
    }

    public List<Receiver> getReceiverList() {
        return receiverList;
    }

    public void setReceiverList(List<Receiver> receiverList) {
        this.receiverList = receiverList;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        MessageSenderUniqueVO that = (MessageSenderUniqueVO) o;

        return new EqualsBuilder()
                .append(tenantId, that.tenantId)
                .append(msgCode, that.msgCode)
                .append(receiverList, that.receiverList)
                .append(ccList, that.ccList)
                .append(noticeTypeList, that.noticeTypeList)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .append(tenantId)
                .append(msgCode)
                .append(receiverList)
                .append(ccList)
                .append(noticeTypeList)
                .toHashCode();
    }
}
