package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/25 16:07
 */
public class IssueAttachmentCombineVO {
    @ApiModelProperty("问题id")
    @Encrypt
    private Long issueId;
    @ApiModelProperty("文件名称")
    private String fileName;
    @ApiModelProperty("源文件md5编码")
    private String guid;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }


    public String getGuid() {
        return guid;
    }

    public void setGuid(String guid) {
        this.guid = guid;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
}