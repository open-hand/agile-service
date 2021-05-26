package io.choerodon.agile.infra.dto.business;

import io.choerodon.agile.api.vo.SprintNameVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.infra.dto.StoryMapVersionDTO;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/6.
 * Email: fuqianghuang01@gmail.com
 */
public class StoryMapStoryDTO {
    @Encrypt
    private Long issueId;

    private String issueNum;

    private String summary;
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    private Boolean completed;
    @Encrypt
    private Long issueTypeId;
    @Encrypt
    private Long statusId;

    private StatusVO statusVO;

    private List<StoryMapVersionDTO> storyMapVersionDTOList;

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

    public void setStoryMapVersionDTOList(List<StoryMapVersionDTO> storyMapVersionDTOList) {
        this.storyMapVersionDTOList = storyMapVersionDTOList;
    }

    public List<StoryMapVersionDTO> getStoryMapVersionDTOList() {
        return storyMapVersionDTOList;
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

    public StatusVO getStatusVO() {
        return statusVO;
    }

    public void setStatusVO(StatusVO statusVO) {
        this.statusVO = statusVO;
    }
}
