package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/7/3 下午4:58
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueCompletedStatusVO {

    public IssueCompletedStatusVO(Long userId, Integer created) {
        this.userId = userId;
        this.created = created;
        this.completed = 0;
    }

    public IssueCompletedStatusVO(Long userId) {
        this.userId = userId;
        this.created = 0;
        this.completed = 0;
    }

    public IssueCompletedStatusVO() {
    }
    @Encrypt
    @ApiModelProperty(value = "用户id")
    private Long userId;
    @ApiModelProperty(value = "用户详情")
    private UserMessageDTO userMessage;
    @ApiModelProperty(value = "工作人")
    private String worker;
    @ApiModelProperty(value = "是否已完成")
    private Integer completed;
    @ApiModelProperty(value = "创建id")
    private Integer created;

    public String getWorker() {
        return worker;
    }

    public void setWorker(String worker) {
        this.worker = worker;
    }

    public Integer getCompleted() {
        return completed;
    }

    public void setCompleted(Integer completed) {
        this.completed = completed;
    }

    public Integer getCreated() {
        return created;
    }

    public void setCreated(Integer created) {
        this.created = created;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public UserMessageDTO getUserMessage() {
        return userMessage;
    }

    public void setUserMessage(UserMessageDTO userMessage) {
        this.userMessage = userMessage;
    }
}
