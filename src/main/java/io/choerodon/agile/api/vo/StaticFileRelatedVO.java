package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/15 10:27
 */
public class StaticFileRelatedVO {
    @ApiModelProperty(value = "静态文件头id")
    @Encrypt
    private Long fileHeaderId;
    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    public Long getFileHeaderId() {
        return fileHeaderId;
    }

    public void setFileHeaderId(Long fileHeaderId) {
        this.fileHeaderId = fileHeaderId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }
}
