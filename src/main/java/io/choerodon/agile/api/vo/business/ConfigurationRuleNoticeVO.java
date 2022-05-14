package io.choerodon.agile.api.vo.business;

import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Objects;
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
    @ApiModelProperty(value = "抄送人id集合")
    private Set<Long> ccUserIds;
    @Encrypt
    @ApiModelProperty(value = "接受人id集合")
    private Set<Long> receiverUserIds;
    @ApiModelProperty(value = "接受类型集合")
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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConfigurationRuleNoticeVO)) return false;
        ConfigurationRuleNoticeVO that = (ConfigurationRuleNoticeVO) o;
        return Objects.equals(getReceiverRoleIds(), that.getReceiverRoleIds()) &&
                Objects.equals(getMemberFieldIds(), that.getMemberFieldIds()) &&
                Objects.equals(getCcUserIds(), that.getCcUserIds()) &&
                Objects.equals(getReceiverUserIds(), that.getReceiverUserIds()) &&
                Objects.equals(getReceiverTypes(), that.getReceiverTypes());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getReceiverRoleIds(), getMemberFieldIds(), getCcUserIds(), getReceiverUserIds(), getReceiverTypes());
    }
}
