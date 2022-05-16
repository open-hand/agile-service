package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.waterfall.PredecessorIssueStatusLinkageVO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:26
 */
public class StatusSettingVO {
    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @ApiModelProperty(value = "名称")
    private String name;
    @ApiModelProperty(value = "编码")
    private String code;
    @ApiModelProperty(value = "类型")
    private String type;
    @ApiModelProperty(value = "乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty(value = "状态转换配置")
    private List<StatusTransferSettingVO> statusTransferSettingVOS;
    @ApiModelProperty(value = "状态字段配置")
    private List<StatusFieldSettingVO> statusFieldSettingVOS;
    @ApiModelProperty(value = "状态通知配置")
    private List<StatusNoticeSettingVO> statusNoticeSettingVOS;
    @ApiModelProperty(value = "状态联动")
    private List<StatusLinkageVO> statusLinkageVOS;
    @ApiModelProperty(value = "状态分支合并配置")
    private StatusBranchMergeSettingVO statusBranchMergeSettingVO;
    @ApiModelProperty(value = "执行状态配置")
    private ExecutionCaseStatusChangeSettingVO executionCaseStatusChangeSettingVO;
    @ApiModelProperty(value = "关联问题状态")
    private List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS;
    @ApiModelProperty(value = "前置问题状态联动")
    private List<PredecessorIssueStatusLinkageVO> predecessorIssueStatusLinkageVOS;

    public StatusBranchMergeSettingVO getStatusBranchMergeSettingVO() {
        return statusBranchMergeSettingVO;
    }

    public void setStatusBranchMergeSettingVO(StatusBranchMergeSettingVO statusBranchMergeSettingVO) {
        this.statusBranchMergeSettingVO = statusBranchMergeSettingVO;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public List<StatusTransferSettingVO> getStatusTransferSettingVOS() {
        return statusTransferSettingVOS;
    }

    public void setStatusTransferSettingVOS(List<StatusTransferSettingVO> statusTransferSettingVOS) {
        this.statusTransferSettingVOS = statusTransferSettingVOS;
    }

    public List<StatusFieldSettingVO> getStatusFieldSettingVOS() {
        return statusFieldSettingVOS;
    }

    public void setStatusFieldSettingVOS(List<StatusFieldSettingVO> statusFieldSettingVOS) {
        this.statusFieldSettingVOS = statusFieldSettingVOS;
    }

    public List<StatusNoticeSettingVO> getStatusNoticeSettingVOS() {
        return statusNoticeSettingVOS;
    }

    public void setStatusNoticeSettingVOS(List<StatusNoticeSettingVO> statusNoticeSettingVOS) {
        this.statusNoticeSettingVOS = statusNoticeSettingVOS;
    }

    public List<StatusLinkageVO> getStatusLinkageVOS() {
        return statusLinkageVOS;
    }

    public void setStatusLinkageVOS(List<StatusLinkageVO> statusLinkageVOS) {
        this.statusLinkageVOS = statusLinkageVOS;
    }

    public ExecutionCaseStatusChangeSettingVO getExecutionCaseStatusChangeSettingVO() {
        return executionCaseStatusChangeSettingVO;
    }

    public void setExecutionCaseStatusChangeSettingVO(ExecutionCaseStatusChangeSettingVO executionCaseStatusChangeSettingVO) {
        this.executionCaseStatusChangeSettingVO = executionCaseStatusChangeSettingVO;
    }

    public List<LinkIssueStatusLinkageVO> getLinkIssueStatusLinkageVOS() {
        return linkIssueStatusLinkageVOS;
    }

    public void setLinkIssueStatusLinkageVOS(List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS) {
        this.linkIssueStatusLinkageVOS = linkIssueStatusLinkageVOS;
    }

    public List<PredecessorIssueStatusLinkageVO> getPredecessorIssueStatusLinkageVOS() {
        return predecessorIssueStatusLinkageVOS;
    }

    public void setPredecessorIssueStatusLinkageVOS(List<PredecessorIssueStatusLinkageVO> predecessorIssueStatusLinkageVOS) {
        this.predecessorIssueStatusLinkageVOS = predecessorIssueStatusLinkageVOS;
    }
}
