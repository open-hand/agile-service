package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
public class IssueMoveVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "状态id")
    @Encrypt
    private Long statusId;

    @ApiModelProperty(value = "冲刺id")
    @Encrypt
    private Long sprintId;

    @ApiModelProperty(value = "看板主键id")
    @Encrypt
    private Long boardId;

    @ApiModelProperty(value = "新列id")
    @Encrypt
    private Long columnId;

    @ApiModelProperty(value = "原始列id")
    @Encrypt
    private Long originColumnId;

    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "true：表示放在某个问题之前；false：表示放在某个问题之后")
    private Boolean before;

    @ApiModelProperty(value = "与before一起用，以该问题为参照物")
    @Encrypt
    private Long outsetIssueId;

    @ApiModelProperty(value = "是否排序")
    private Boolean rankFlag;

    @ApiModelProperty(value = "是否排序")
    private Long projectId;
    @ApiModelProperty(value = "错误信息")
    private String errorMsg;

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Boolean getRankFlag() {
        return rankFlag;
    }

    public void setRankFlag(Boolean rankFlag) {
        this.rankFlag = rankFlag;
    }

    public Boolean getBefore() {
        return before;
    }

    public void setBefore(Boolean before) {
        this.before = before;
    }

    public Long getSprintId() {
        return sprintId;
    }

    public void setSprintId(Long sprintId) {
        this.sprintId = sprintId;
    }

    public Long getOutsetIssueId() {
        return outsetIssueId;
    }

    public void setOutsetIssueId(Long outsetIssueId) {
        this.outsetIssueId = outsetIssueId;
    }

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setBoardId(Long boardId) {
        this.boardId = boardId;
    }

    public Long getBoardId() {
        return boardId;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public Long getColumnId() {
        return columnId;
    }

    public void setColumnId(Long columnId) {
        this.columnId = columnId;
    }

    public Long getOriginColumnId() {
        return originColumnId;
    }

    public void setOriginColumnId(Long originColumnId) {
        this.originColumnId = originColumnId;
    }
}
