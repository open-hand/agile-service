package io.choerodon.agile.infra.dto;

import java.util.List;

/**
 * Creator: ChangpingShi0213@gmail.com
 * Date:  16:26 2018/9/4
 * Description:
 */
public class IssueTypeDistributeDTO {
    private String typeCode;

    private List<IssueStatus> issueStatus;

    private String name;

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public List<IssueStatus> getIssueStatus() {
        return issueStatus;
    }

    public void setIssueStatus(List<IssueStatus> issueStatus) {
        this.issueStatus = issueStatus;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
