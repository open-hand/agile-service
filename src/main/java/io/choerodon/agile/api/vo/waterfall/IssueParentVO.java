package io.choerodon.agile.api.vo.waterfall;

import io.choerodon.agile.api.vo.IssueTypeVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/2/17
 */
public class IssueParentVO {

    @Encrypt
    private Long issueId;

    private String summary;

    private String issueNum;

    private String description;

    private String typeCode;

    @Encrypt
    private Long issueTypeId;

    private Boolean starBeacon;

    private IssueTypeVO issueTypeVO;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Boolean getStarBeacon() {
        return starBeacon;
    }

    public void setStarBeacon(Boolean starBeacon) {
        this.starBeacon = starBeacon;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }
}
