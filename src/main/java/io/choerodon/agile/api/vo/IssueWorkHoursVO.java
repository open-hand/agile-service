package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.dto.UserDTO;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.math.BigDecimal;

/**
 * @author zhaotianxin
 * @date 2021-11-15 10:32
 */
public class IssueWorkHoursVO {

    @Encrypt
    private Long userId;

    private Long projectId;

    private ProjectVO projectVO;

    private UserDTO userDTO;

    @ApiModelProperty("工时")
    private BigDecimal workTime;

    @ApiModelProperty("原始预估工时")
    private BigDecimal estimateTime;

    @ApiModelProperty("历史累计工时")
    private BigDecimal cumulativeWorkTime;

    @ApiModelProperty("偏差率")
    private BigDecimal deviationRate;

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public UserDTO getUserDTO() {
        return userDTO;
    }

    public void setUserDTO(UserDTO userDTO) {
        this.userDTO = userDTO;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public void setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
    }

    public BigDecimal getEstimateTime() {
        return estimateTime;
    }

    public void setEstimateTime(BigDecimal estimateTime) {
        this.estimateTime = estimateTime;
    }

    public BigDecimal getCumulativeWorkTime() {
        return cumulativeWorkTime;
    }

    public void setCumulativeWorkTime(BigDecimal cumulativeWorkTime) {
        this.cumulativeWorkTime = cumulativeWorkTime;
    }

    public BigDecimal getDeviationRate() {
        return deviationRate;
    }

    public void setDeviationRate(BigDecimal deviationRate) {
        this.deviationRate = deviationRate;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public ProjectVO getProjectVO() {
        return projectVO;
    }

    public void setProjectVO(ProjectVO projectVO) {
        this.projectVO = projectVO;
    }
}
