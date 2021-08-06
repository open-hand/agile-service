package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-08-02
 */
public class ConfigurationRuleNoticeVO extends ConfigurationRuleSettingVO {

    @ApiModelProperty("抄送人")
    private List<UserDTO> ccList;

    @Encrypt
    @ApiModelProperty("通知角色id")
    private Set<Long> receiverRoleIds;

    @ApiModelProperty("通知对象")
    private List<UserDTO> receiverList;

    @Encrypt
    @ApiModelProperty("通知自定义人员对象")
    private Set<Long> memberFieldIds;
    @Encrypt
    private Set<Long> ccUserIds;
    @Encrypt
    private Set<Long> receiverUserIds;

    private Set<String> receiverTypes;

    public Set<String> getReceiverTypes() {
        return receiverTypes;
    }

    public void setReceiverTypes(Set<String> receiverTypes) {
        this.receiverTypes = receiverTypes;
    }

    public Set<Long> getReceiverRoleIds() {
        return receiverRoleIds;
    }

    public void setReceiverRoleIds(Set<Long> receiverRoleIds) {
        this.receiverRoleIds = receiverRoleIds;
    }

    public Set<Long> getMemberFieldIds() {
        return memberFieldIds;
    }

    public void setMemberFieldIds(Set<Long> memberFieldIds) {
        this.memberFieldIds = memberFieldIds;
    }

    public Set<Long> getCcUserIds() {
        return ccUserIds;
    }

    public void setCcUserIds(Set<Long> ccUserIds) {
        this.ccUserIds = ccUserIds;
    }

    public Set<Long> getReceiverUserIds() {
        return receiverUserIds;
    }

    public void setReceiverUserIds(Set<Long> receiverUserIds) {
        this.receiverUserIds = receiverUserIds;
    }

    public List<UserDTO> getCcList() {
        return ccList;
    }

    public void setCcList(List<UserDTO> ccList) {
        this.ccList = ccList;
    }

    public List<UserDTO> getReceiverList() {
        return receiverList;
    }

    public void setReceiverList(List<UserDTO> receiverList) {
        this.receiverList = receiverList;
    }
}
