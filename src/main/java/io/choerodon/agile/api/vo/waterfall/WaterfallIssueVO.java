package io.choerodon.agile.api.vo.waterfall;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/2/17
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WaterfallIssueVO {

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "项目id")
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

    @ApiModelProperty(value = "关联的瀑布工作项id")
    @Encrypt
    private Long wfIssueRelId;

    @ApiModelProperty(value = "交付物")
    private List<WfDeliverableVO> wfDeliverableVOS;

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

    public Long getWfIssueRelId() {
        return wfIssueRelId;
    }

    public void setWfIssueRelId(Long wfIssueRelId) {
        this.wfIssueRelId = wfIssueRelId;
    }

    public List<WfDeliverableVO> getWfDeliverableVOS() {
        return wfDeliverableVOS;
    }

    public void setWfDeliverableVOS(List<WfDeliverableVO> wfDeliverableVOS) {
        this.wfDeliverableVOS = wfDeliverableVOS;
    }
}
