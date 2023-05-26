package io.choerodon.agile.api.vo;

import java.util.Date;
import javax.validation.constraints.NotBlank;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * 项目字段
 *
 * @author 汪翔 2023-05-15
 */
public class FileVO {

    @ApiModelProperty("表ID，主键，供其他表做外键")
    @Encrypt
    private Long fileId;
    @ApiModelProperty("附件集UUID")
    private String attachmentUuid;
    @ApiModelProperty("上传目录")
    private String directory;
    @ApiModelProperty("文件地址")
    private String fileUrl;

    @ApiModelProperty("文件类型")
    private String fileType;
    @ApiModelProperty("文件名称")

    private String fileName;
    @ApiModelProperty("文件大小")
    private Long fileSize;
    @NotBlank(message = "文件Bucket不能为空")

    @ApiModelProperty("文件目录")
    private String bucketName;
    @ApiModelProperty("对象KEY")

    private String fileKey;

    @ApiModelProperty("租户Id")
    private Long tenantId;

    @ApiModelProperty("文件MD5")
    private String md5;

    private Date lastUpdateDate;
    private Long lastUpdatedBy;
    private Date creationDate;
    private Long createdBy;

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public String getAttachmentUuid() {
        return attachmentUuid;
    }

    public void setAttachmentUuid(String attachmentUuid) {
        this.attachmentUuid = attachmentUuid;
    }

    public String getDirectory() {
        return directory;
    }

    public void setDirectory(String directory) {
        this.directory = directory;
    }

    public String getFileUrl() {
        return fileUrl;
    }

    public void setFileUrl(String fileUrl) {
        this.fileUrl = fileUrl;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Long getFileSize() {
        return fileSize;
    }

    public void setFileSize(Long fileSize) {
        this.fileSize = fileSize;
    }

    public String getBucketName() {
        return bucketName;
    }

    public void setBucketName(String bucketName) {
        this.bucketName = bucketName;
    }

    public String getFileKey() {
        return fileKey;
    }

    public void setFileKey(String fileKey) {
        this.fileKey = fileKey;
    }

    public Long getTenantId() {
        return tenantId;
    }

    public void setTenantId(Long tenantId) {
        this.tenantId = tenantId;
    }

    public String getMd5() {
        return md5;
    }

    public void setMd5(String md5) {
        this.md5 = md5;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public void setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
    }

    public Long getLastUpdatedBy() {
        return lastUpdatedBy;
    }

    public void setLastUpdatedBy(Long lastUpdatedBy) {
        this.lastUpdatedBy = lastUpdatedBy;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }
}
