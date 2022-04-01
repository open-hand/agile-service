package io.choerodon.agile.api.vo;

import io.choerodon.agile.api.vo.waterfall.PredecessorIssueStatusLinkageVO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:26
 */
public class StatusSettingVO {
    @Encrypt
    private Long id;

    private String name;

    private String code;

    private String type;

    private Long objectVersionNumber;

    private List<StatusTransferSettingVO> statusTransferSettingVOS;

    private List<StatusFieldSettingVO> statusFieldSettingVOS;

    private List<StatusNoticeSettingVO> statusNoticeSettingVOS;

    private List<StatusLinkageVO> statusLinkageVOS;

    private StatusBranchMergeSettingVO statusBranchMergeSettingVO;

    private ExecutionCaseStatusChangeSettingVO executionCaseStatusChangeSettingVO;

    private List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS;

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
