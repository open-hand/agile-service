package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-02 11:17
 */
public class IssueFilterParamVO {

    @Encrypt
    @ApiModelProperty(value = "issueId")
    private Long issueId;

    @ApiModelProperty(value = "搜索内容")
    private String content;

    @ApiModelProperty(value = "创建时间")
    private Boolean self;

    @ApiModelProperty(value = "只查询活跃的冲刺的问题")
    private Boolean onlyActiveSprint;

    @ApiModelProperty(value = "issueNum")
    private String issueNum;

    @Encrypt
    @ApiModelProperty(value = "按用户Id筛选")
    private List<Long> userIds;


    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Boolean getSelf() {
        return self;
    }

    public void setSelf(Boolean self) {
        this.self = self;
    }

    public Boolean getOnlyActiveSprint() {
        return onlyActiveSprint;
    }

    public void setOnlyActiveSprint(Boolean onlyActiveSprint) {
        this.onlyActiveSprint = onlyActiveSprint;
    }

    public String getIssueNum() {
        return issueNum;
    }

    public void setIssueNum(String issueNum) {
        this.issueNum = issueNum;
    }

    public List<Long> getUserIds() {
        return userIds;
    }

    public void setUserIds(List<Long> userIds) {
        this.userIds = userIds;
    }
}
