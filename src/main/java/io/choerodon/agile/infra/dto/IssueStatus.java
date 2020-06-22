package io.choerodon.agile.infra.dto;

import io.choerodon.agile.infra.constants.EncryptionConstant;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Creator: ChangpingShi0213@gmail.com
 * Date:  15:00 2018/9/5
 * Description:
 */
public class IssueStatus {
    private Integer issueNum;
    private String categoryCode;
    @Encrypt/*(EncryptionConstant.FD_STATUS)*/
    private Long statusId;

    public Integer getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(Integer issueNum) {
        this.issueNum = issueNum;
    }

    public String getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(String categoryCode) {
        this.categoryCode = categoryCode;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }
}
