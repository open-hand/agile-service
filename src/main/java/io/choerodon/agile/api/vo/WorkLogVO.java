package io.choerodon.agile.api.vo;


import java.math.BigDecimal;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.business.WorkHoursLabelVO;
import io.choerodon.agile.api.vo.business.WorkHoursTypeVO;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/18.
 * Email: fuqianghuang01@gmail.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WorkLogVO {

    @ApiModelProperty(value = "工作日志主键id")
    @Encrypt
    private Long logId;

    @ApiModelProperty(value = "工作时间")
    private BigDecimal workTime;

    @ApiModelProperty(value = "开始时间")
    private Date startDate;

    @ApiModelProperty(value = "描述")
    private String description;

    @ApiModelProperty(value = "问题id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "剩余的估计类别字段")
    private String residualPrediction;

    @ApiModelProperty(value = "预估时间")
    private BigDecimal predictionTime;

    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Long createdBy;

    @ApiModelProperty(value = "用户名称")
    private String userName;

    @ApiModelProperty(value = "最后更新时间")
    private Date lastUpdateDate;

    @ApiModelProperty(value = "用户头像url")
    private String userImageUrl;

    @ApiModelProperty(value = "用户真实姓名")
    private String realName;

    @ApiModelProperty(value = "用户登录名")
    private String loginName;
    
    @ApiModelProperty(value = "工时日志类型")
    private WorkHoursTypeVO workHoursType;
    
    @ApiModelProperty(value = "工时日志标签")
    private WorkHoursLabelVO workHoursLabel;

    public String getUserImageUrl() {
        return userImageUrl;
    }

    public WorkLogVO setUserImageUrl(String userImageUrl) {
        this.userImageUrl = userImageUrl;
        return this;
    }

    public String getRealName() {
        return realName;
    }

    public WorkLogVO setRealName(String realName) {
        this.realName = realName;
        return this;
    }

    public String getLoginName() {
        return loginName;
    }

    public WorkLogVO setLoginName(String loginName) {
        this.loginName = loginName;
        return this;
    }

    public Long getLogId() {
        return logId;
    }

    public WorkLogVO setLogId(Long logId) {
        this.logId = logId;
        return this;
    }

    public WorkLogVO setWorkTime(BigDecimal workTime) {
        this.workTime = workTime;
        return this;
    }

    public BigDecimal getWorkTime() {
        return workTime;
    }

    public WorkLogVO setStartDate(Date startDate) {
        this.startDate = startDate;
        return this;
    }

    public Date getStartDate() {
        return startDate;
    }

    public String getDescription() {
        return description;
    }

    public WorkLogVO setDescription(String description) {
        this.description = description;
        return this;
    }

    public Long getIssueId() {
        return issueId;
    }

    public WorkLogVO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public WorkLogVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public WorkLogVO setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
        return this;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public WorkLogVO setResidualPrediction(String residualPrediction) {
        this.residualPrediction = residualPrediction;
        return this;
    }

    public String getResidualPrediction() {
        return residualPrediction;
    }

    public WorkLogVO setPredictionTime(BigDecimal predictionTime) {
        this.predictionTime = predictionTime;
        return this;
    }

    public BigDecimal getPredictionTime() {
        return predictionTime;
    }

    public WorkLogVO setCreatedBy(Long createdBy) {
        this.createdBy = createdBy;
        return this;
    }

    public Long getCreatedBy() {
        return createdBy;
    }

    public String getUserName() {
        return userName;
    }

    public WorkLogVO setUserName(String userName) {
        this.userName = userName;
        return this;
    }

    public Date getLastUpdateDate() {
        return lastUpdateDate;
    }

    public WorkLogVO setLastUpdateDate(Date lastUpdateDate) {
        this.lastUpdateDate = lastUpdateDate;
        return this;
    }

    public WorkHoursTypeVO getWorkHoursType() {
        return workHoursType;
    }

    public WorkLogVO setWorkHoursType(WorkHoursTypeVO workHoursType) {
        this.workHoursType = workHoursType;
        return this;
    }

    public WorkHoursLabelVO getWorkHoursLabel() {
        return workHoursLabel;
    }

    public WorkLogVO setWorkHoursLabel(WorkHoursLabelVO workHoursLabel) {
        this.workHoursLabel = workHoursLabel;
        return this;
    }
}
