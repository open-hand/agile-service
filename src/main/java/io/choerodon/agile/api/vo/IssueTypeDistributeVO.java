package io.choerodon.agile.api.vo;

import java.util.List;

import io.choerodon.agile.infra.dto.IssueStatus;
import io.swagger.annotations.ApiModelProperty;

/**
 * Creator: ChangpingShi0213@gmail.com
 * Date:  16:12 2018/9/4
 * Description:
 */
public class IssueTypeDistributeVO {

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "该问题类型下的状态类别")
    private List<IssueStatus> issueStatus;

    @ApiModelProperty("问题类型名称")
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
