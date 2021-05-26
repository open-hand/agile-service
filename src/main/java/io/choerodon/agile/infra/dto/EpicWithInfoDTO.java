package io.choerodon.agile.infra.dto;

import io.choerodon.agile.api.vo.StatusVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

public class EpicWithInfoDTO {

    @Encrypt
    private Long issueId;

    private String issueNum;

    private String typeCode;

    private String summary;

    private String epicName;

    private String epicRank;

    private Long epicRankObjectVersionNumber;

    private StatusVO statusVO;

    private Long objectVersionNumber;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getEpicName() {
        return epicName;
    }

    public String getEpicRank() {
        return epicRank;
    }

    public void setEpicRank(String epicRank) {
        this.epicRank = epicRank;
    }

    public Long getEpicRankObjectVersionNumber() {
        return epicRankObjectVersionNumber;
    }

    public void setEpicRankObjectVersionNumber(Long epicRankObjectVersionNumber) {
        this.epicRankObjectVersionNumber = epicRankObjectVersionNumber;
    }

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}
