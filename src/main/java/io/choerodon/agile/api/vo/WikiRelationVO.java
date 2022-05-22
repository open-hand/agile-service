package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
public class WikiRelationVO {

    @Encrypt
    @ApiModelProperty(value = "id")
    private Long id;
    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @Encrypt
    @ApiModelProperty(value = "问题id")
    private Long issueId;
    @ApiModelProperty(value = "wiki名称")
    private String wikiName;
    @ApiModelProperty(value = "wiki url")
    private String wikiUrl;

    @Encrypt
    @ApiModelProperty(value = "空间id")
    private Long spaceId;
    @ApiModelProperty(value = "工作空间")
    private WorkSpaceVO workSpaceVO;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getWikiName() {
        return wikiName;
    }

    public void setWikiName(String wikiName) {
        this.wikiName = wikiName;
    }

    public String getWikiUrl() {
        return wikiUrl;
    }

    public void setWikiUrl(String wikiUrl) {
        this.wikiUrl = wikiUrl;
    }

    public void setSpaceId(Long spaceId) {
        this.spaceId = spaceId;
    }

    public Long getSpaceId() {
        return spaceId;
    }

    public void setWorkSpaceVO(WorkSpaceVO workSpaceVO) {
        this.workSpaceVO = workSpaceVO;
    }

    public WorkSpaceVO getWorkSpaceVO() {
        return workSpaceVO;
    }
}
