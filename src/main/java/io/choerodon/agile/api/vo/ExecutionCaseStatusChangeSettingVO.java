package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author zhaotianxin
 * @date 2021-05-11 11:30
 */
public class ExecutionCaseStatusChangeSettingVO {

    @Encrypt
    @ApiModelProperty("id")
    private Long id;

    @Encrypt
    @ApiModelProperty("敏捷问题类型id")
    private Long agileIssueTypeId;

    @Encrypt
    @ApiModelProperty("敏捷状态id")
    private Long agileStatusId;

    @Encrypt
    @ApiModelProperty("测试状态id")
    private Long testStatusId;
    @ApiModelProperty("乐观锁")
    private Long objectVersionNumber;
    @ApiModelProperty("测试状态")
    private TestStatusVO testStatusVO;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getAgileIssueTypeId() {
        return agileIssueTypeId;
    }

    public void setAgileIssueTypeId(Long agileIssueTypeId) {
        this.agileIssueTypeId = agileIssueTypeId;
    }

    public Long getAgileStatusId() {
        return agileStatusId;
    }

    public void setAgileStatusId(Long agileStatusId) {
        this.agileStatusId = agileStatusId;
    }

    public Long getTestStatusId() {
        return testStatusId;
    }

    public void setTestStatusId(Long testStatusId) {
        this.testStatusId = testStatusId;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }

    public TestStatusVO getTestStatusVO() {
        return testStatusVO;
    }

    public void setTestStatusVO(TestStatusVO testStatusVO) {
        this.testStatusVO = testStatusVO;
    }
}
