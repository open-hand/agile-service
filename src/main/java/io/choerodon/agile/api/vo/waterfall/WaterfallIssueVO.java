package io.choerodon.agile.api.vo.waterfall;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/2/17
 */
public class WaterfallIssueVO {

    @Encrypt
    private Long issueId;

    private Long projectId;

    @ApiModelProperty(value = "所属父级")
    @Encrypt(ignoreValue = {"0"})
    private Long parentId;

    @ApiModelProperty(value = "所属父级VO")
    private IssueParentVO issueParentVO;

    @ApiModelProperty(value = "进度")
    private Integer progress;

    @ApiModelProperty(value = "是否为父级")
    private boolean isParent;

    @ApiModelProperty(value = "子级")
    private List<ChildIssueListVO> childIssueList;

    @ApiModelProperty(value = "同父级的子级")
    private List<ChildIssueListVO> sameWfParentIssueList;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public IssueParentVO getIssueParentVO() {
        return issueParentVO;
    }

    public void setIssueParentVO(IssueParentVO issueParentVO) {
        this.issueParentVO = issueParentVO;
    }

    public Integer getProgress() {
        return progress;
    }

    public void setProgress(Integer progress) {
        this.progress = progress;
    }

    public boolean isParent() {
        return isParent;
    }

    public void setParent(boolean parent) {
        isParent = parent;
    }

    public List<ChildIssueListVO> getChildIssueList() {
        return childIssueList;
    }

    public void setChildIssueList(List<ChildIssueListVO> childIssueList) {
        this.childIssueList = childIssueList;
    }

    public List<ChildIssueListVO> getSameWfParentIssueList() {
        return sameWfParentIssueList;
    }

    public void setSameWfParentIssueList(List<ChildIssueListVO> sameWfParentIssueList) {
        this.sameWfParentIssueList = sameWfParentIssueList;
    }
}
