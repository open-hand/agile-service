package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;

import java.util.List;

/**
 * @author peng.jiang@hand-china.com
 */
public class StateMachineSchemeConfigViewVO {
    @ApiModelProperty(value = "状态机DTO")
    private StatusMachineVO statusMachineVO;
    @ApiModelProperty(value = "问题类型列表")
    private List<IssueTypeVO> issueTypeVOS;

    public StatusMachineVO getStatusMachineVO() {
        return statusMachineVO;
    }

    public void setStatusMachineVO(StatusMachineVO statusMachineVO) {
        this.statusMachineVO = statusMachineVO;
    }

    public List<IssueTypeVO> getIssueTypeVOS() {
        return issueTypeVOS;
    }

    public void setIssueTypeVOS(List<IssueTypeVO> issueTypeVOS) {
        this.issueTypeVOS = issueTypeVOS;
    }
}

