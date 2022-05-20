package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 10:02
 */
public class StatusTransferSettingCreateVO {
    @ApiModelProperty(value = "类型")
    private String type;

    @Encrypt
    @ApiModelProperty(value = "用户id")
    private List<Long> userIds;
    @ApiModelProperty(value = "是否校验订阅完成")
    private Boolean isVerifySubissueCompleted;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(List<Long> userIds) {
        this.userIds = userIds;
    }

    public Boolean getVerifySubissueCompleted() {
        return isVerifySubissueCompleted;
    }

    public void setVerifySubissueCompleted(Boolean verifySubissueCompleted) {
        isVerifySubissueCompleted = verifySubissueCompleted;
    }
}
