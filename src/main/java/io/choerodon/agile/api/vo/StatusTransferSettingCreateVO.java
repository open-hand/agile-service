package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 10:02
 */
public class StatusTransferSettingCreateVO {

    private String type;

    @Encrypt
    private List<Long> userIds;

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
