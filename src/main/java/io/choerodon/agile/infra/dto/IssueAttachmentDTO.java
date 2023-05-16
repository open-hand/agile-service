package io.choerodon.agile.infra.dto;


import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
@Table(name = "agile_issue_attachment")
@ModifyAudit
@VersionAudit
public class IssueAttachmentDTO extends AuditDomain {

    @Id
    @GeneratedValue
    private Long attachmentId;

    private Long issueId;

    private Long commentId;

    private String url;

    private String fileName;

    private Long projectId;

    private Long createdBy;

    @ApiModelProperty("fileKey")
    @Transient
    private String fileKey;
    @ApiModelProperty("文件的大小")
    @Transient
    private Long size;

    // 前端onlyoffice展示时需要用到的字段
    /**
     * “fileType”：“docx”，
     * “key”：“Khirz6zTPdfd7”，
     * title”：“示例文档 Title.docx”，
     * “url”：“https://example.com/url -to-example-document.docx"
     */
    @ApiModelProperty("文件的类型（根据后缀来判断）")
    @Transient
    private String fileType;
    @ApiModelProperty("onlyOffice用来标识文件的唯一性")
    @Transient
    private String fileId;

    @Transient
    @ApiModelProperty("该附件是否支持wps预览（部分旧数据不支持）")
    private Boolean supportWps;


    public Long getAttachmentId() {
        return attachmentId;
    }

    public void setAttachmentId(Long attachmentId) {
        this.attachmentId = attachmentId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getCommentId() {
        return commentId;
    }

    public void setCommentId(Long commentId) {
        this.commentId = commentId;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    @Override
    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

    @Override
    public Long getCreatedBy() {
        return createdBy;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public String getFileKey() {
        return fileKey;
    }

    public void setFileKey(String fileKey) {
        this.fileKey = fileKey;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }

    public String getFileId() {
        return fileId;
    }

    public void setFileId(String fileId) {
        this.fileId = fileId;
    }

    public Boolean getSupportWps() {
        return supportWps;
    }

    public void setSupportWps(Boolean supportWps) {
        this.supportWps = supportWps;
    }
}
