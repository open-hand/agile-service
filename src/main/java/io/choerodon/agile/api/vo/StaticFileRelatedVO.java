package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/15 10:27
 */
public class StaticFileRelatedVO {
    @ApiModelProperty(value = "静态文件头id")
    @Encrypt
    private List<Long> fileHeaderIds;
    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    public List<Long> getFileHeaderIds() {
        return fileHeaderIds;
    }

    public void setFileHeaderIds(List<Long> fileHeaderIds) {
        this.fileHeaderIds = fileHeaderIds;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }
}
