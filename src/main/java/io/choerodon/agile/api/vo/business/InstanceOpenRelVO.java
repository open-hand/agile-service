package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/24
 */
public class InstanceOpenRelVO {

    @ApiModelProperty(value = "主键id")
    @Encrypt
    private Long id;

    @ApiModelProperty(value = "需求/工作项id")
    @Encrypt
    private Long instanceId;

    @ApiModelProperty(value = "实例类型：issue/backlog")
    private String instanceType;

    @ApiModelProperty(value = "第三方实例id")
    private Long openInstanceId;

    @ApiModelProperty(value = "第三方实例编号")
    private String openInstanceNum;

    @ApiModelProperty(value = "来源：如yqcloud等")
    private String source;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "组织id")
    private Long organizationId;

    public InstanceOpenRelVO() {
    }

    public InstanceOpenRelVO(Long instanceId,
                             String instanceType,
                             Long openInstanceId) {
        this.instanceId = instanceId;
        this.instanceType = instanceType;
        this.openInstanceId = openInstanceId;
    }

    public InstanceOpenRelVO(Long instanceId,
                             String instanceType,
                             Long openInstanceId,
                             String openInstanceNum,
                             String source,
                             Long projectId,
                             Long organizationId) {
        this.instanceId = instanceId;
        this.instanceType = instanceType;
        this.openInstanceId = openInstanceId;
        this.openInstanceNum = openInstanceNum;
        this.source = source;
        this.projectId = projectId;
        this.organizationId = organizationId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public String getInstanceType() {
        return instanceType;
    }

    public void setInstanceType(String instanceType) {
        this.instanceType = instanceType;
    }

    public Long getOpenInstanceId() {
        return openInstanceId;
    }

    public void setOpenInstanceId(Long openInstanceId) {
        this.openInstanceId = openInstanceId;
    }

    public String getOpenInstanceNum() {
        return openInstanceNum;
    }

    public void setOpenInstanceNum(String openInstanceNum) {
        this.openInstanceNum = openInstanceNum;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}
