package io.choerodon.agile.api.vo.business;


import io.choerodon.agile.api.vo.VersionIssueRelVO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/3.
 * Email: fuqianghuang01@gmail.com
 */
public class StoryMapDragVO {

    @ApiModelProperty(value = "要关联的史诗id")
    @Encrypt(ignoreValue = {"0"})
    private Long epicId;

    @ApiModelProperty(value = "要关联的特性id")
    @Encrypt(ignoreValue = {"0"})
    private Long featureId;

    @ApiModelProperty(value = "要关联的版本id")
    @Encrypt(ignoreValue = {"0"})
    private Long versionId;

    @ApiModelProperty(value = "要关联的冲刺Id")
    @Encrypt(ignoreValue = {"0"})
    private Long sprintId;

    @ApiModelProperty(value = "问题id列表，移动到冲刺，配合sprintId使用")
    @Encrypt(ignoreValue = {"0"})
    private List<Long> sprintIssueIds;


    @ApiModelProperty(value = "问题id列表，移动到史诗，配合epicId使用")
    @Encrypt
    private List<Long> epicIssueIds;

    @ApiModelProperty(value = "问题id列表，移动到特性，配合featureId使用")
    @Encrypt
    private List<Long> featureIssueIds;

    @ApiModelProperty(value = "问题id列表，移动到版本，配合versionId使用")
    @Encrypt
    private List<Long> versionIssueIds;

    @ApiModelProperty(value = "要删除的版本与问题关联数据")
    private List<VersionIssueRelVO> versionIssueRelVOList;

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

    public Long getVersionId() {
        return versionId;
    }

    public void setVersionId(Long versionId) {
        this.versionId = versionId;
    }

    public List<Long> getEpicIssueIds() {
        return epicIssueIds;
    }

    public void setEpicIssueIds(List<Long> epicIssueIds) {
        this.epicIssueIds = epicIssueIds;
    }

    public List<Long> getFeatureIssueIds() {
        return featureIssueIds;
    }

    public void setFeatureIssueIds(List<Long> featureIssueIds) {
        this.featureIssueIds = featureIssueIds;
    }

    public List<Long> getVersionIssueIds() {
        return versionIssueIds;
    }

    public void setVersionIssueIds(List<Long> versionIssueIds) {
        this.versionIssueIds = versionIssueIds;
    }

    public void setVersionIssueRelVOList(List<VersionIssueRelVO> versionIssueRelVOList) {
        this.versionIssueRelVOList = versionIssueRelVOList;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public List<Long> getSprintIssueIds() {
        return sprintIssueIds;
    }

    public void setSprintIssueIds(List<Long> sprintIssueIds) {
        this.sprintIssueIds = sprintIssueIds;
    }

    public List<VersionIssueRelVO> getVersionIssueRelVOList() {
        return versionIssueRelVOList;
    }
}
