package io.choerodon.agile.api.vo;



import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
public class IssueAttachmentVO {

    @ApiModelProperty(value = "附件主键id")
    @Encrypt
    private Long attachmentId;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "评论id")
    @Encrypt
    private Long commentId;

    @ApiModelProperty(value = "附件url")
    private String url;

    @ApiModelProperty(value = "文件名称")
    private String fileName;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "创建人")
    @Encrypt
    private Long createdBy;

    @ApiModelProperty("fileKey")
    private String fileKey;

    // 前端onlyoffice展示时需要用到的字段
    /**
     * “fileType”：“docx”，
     * “key”：“Khirz6zTPdfd7”，
     * title”：“示例文档 Title.docx”，
     * “url”：“https://example.com/url -to-example-document.docx"
     */
    @ApiModelProperty("文件的类型（根据后缀来判断）")
    private String fileType;
    @ApiModelProperty("onlyOffice用来标识文件的唯一性")
    private String key;
    @ApiModelProperty("文件名")
    private String title;

    @ApiModelProperty("文件的大小")
    private Long size;

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

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public void setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
    }

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

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }
}