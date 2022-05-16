package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/6.
 * Email: fuqianghuang01@gmail.com
 */
public class StoryMapStoryVO {

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "问题编号")
    private String issueNum;
    @ApiModelProperty(value = "概要")
    private String summary;

    @Encrypt
    @ApiModelProperty(value = "史诗id")
    private Long epicId;

    @Encrypt
    @ApiModelProperty(value = "特性id")
    private Long featureId;
    @ApiModelProperty(value = "是否已完成")
    private Boolean completed;

    @Encrypt
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;

    @Encrypt
    @ApiModelProperty(value = "状态id")
    private Long statusId;
    @ApiModelProperty(value = "问题类型")
    private IssueTypeVO issueTypeVO;
    @ApiModelProperty(value = "状态mao")
    private StatusVO statusMapVO;
    @ApiModelProperty(value = "故事地图")
    private List<StoryMapVersionVO> storyMapVersionVOList;
    @ApiModelProperty(value = "冲刺")
    private List<SprintNameVO> storyMapSprintList;

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

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public Long getEpicId() {
        return epicId;
    }

    public void setEpicId(Long epicId) {
        this.epicId = epicId;
    }

    public Long getFeatureId() {
        return featureId;
    }

    public void setFeatureId(Long featureId) {
        this.featureId = featureId;
    }

    public void setStoryMapVersionVOList(List<StoryMapVersionVO> storyMapVersionVOList) {
        this.storyMapVersionVOList = storyMapVersionVOList;
    }

    public List<StoryMapVersionVO> getStoryMapVersionVOList() {
        return storyMapVersionVOList;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public IssueTypeVO getIssueTypeVO() {
        return issueTypeVO;
    }

    public void setIssueTypeVO(IssueTypeVO issueTypeVO) {
        this.issueTypeVO = issueTypeVO;
    }

    public StatusVO getStatusVO() {
        return statusMapVO;
    }

    public void setStatusVO(StatusVO statusMapVO) {
        this.statusMapVO = statusMapVO;
    }

    public void setCompleted(Boolean completed) {
        this.completed = completed;
    }

    public Boolean getCompleted() {
        return completed;
    }

    public List<SprintNameVO> getStoryMapSprintList() {
        return storyMapSprintList;
    }

    public void setStoryMapSprintList(List<SprintNameVO> storyMapSprintList) {
        this.storyMapSprintList = storyMapSprintList;
    }
}
