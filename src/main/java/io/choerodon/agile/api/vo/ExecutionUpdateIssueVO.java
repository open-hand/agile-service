package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2021-05-10 20:29
 */
public class ExecutionUpdateIssueVO {
    @Encrypt
    @ApiModelProperty("冲刺id")
    private Long sprintId;
    @ApiModelProperty("问题类型状态map")
    private Map<Long, Long> issueTypeStatusMap;

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public Map<Long, Long> getIssueTypeStatusMap() {
        return issueTypeStatusMap;
    }

    public void setIssueTypeStatusMap(Map<Long, Long> issueTypeStatusMap) {
        this.issueTypeStatusMap = issueTypeStatusMap;
    }
}
