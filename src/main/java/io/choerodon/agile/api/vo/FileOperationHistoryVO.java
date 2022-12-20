package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Date;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */

public class FileOperationHistoryVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    @ApiModelProperty(value = "用户id")
    private Long userId;

    @ApiModelProperty(value = "导入动作：upload_file")
    private String action;

    @ApiModelProperty(value = "导入成功数量")
    private Long successCount;

    @ApiModelProperty(value = "导入失败数量")
    private Long failCount;

    @ApiModelProperty(value = "状态")
    private String status;

    @ApiModelProperty(value = "导入失败后，上传错误文件url")
    private String fileUrl;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "创建时间")
    private Date creationDate;

    @ApiModelProperty(value = "最后更新日期")
    private Date lastUpdateDate;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public Long getSuccessCount() {
        return successCount;
    }

    public void setSuccessCount(Long successCount) {
        this.successCount = successCount;
    }

    public Long getFailCount() {
        return failCount;
    }

    public void setFailCount(Long failCount) {
        this.failCount = failCount;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

    public void setFileUrl(String fileUrl) {
        this.fileUrl = fileUrl;
    }

    public String getFileUrl() {
        return fileUrl;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public String toString() {
        return "FileOperationHistoryVO{" +
                "id=" + id +
                ", projectId=" + projectId +
                ", organizationId=" + organizationId +
                ", userId=" + userId +
                ", action='" + action + '\'' +
                ", successCount=" + successCount +
                ", failCount=" + failCount +
                ", status='" + status + '\'' +
                ", fileUrl='" + fileUrl + '\'' +
                ", objectVersionNumber=" + objectVersionNumber +
                ", creationDate=" + creationDate +
                ", lastUpdateDate=" + lastUpdateDate +
                '}';
    }
}
