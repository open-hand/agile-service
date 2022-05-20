package io.choerodon.agile.api.vo;

import java.util.Set;

import io.swagger.annotations.ApiModelProperty;
import org.springframework.context.ApplicationEvent;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/22 下午4:02
 */
public class NoticeEventVO extends ApplicationEvent {
    /**
     * Create a new ApplicationEvent.
     *
     * @param source the object on which the event initially occurred (never {@code null})
     */
    public NoticeEventVO(Object source) {
        super(source);
    }

    public NoticeEventVO(Object source,
                         String component,
                         String event,
                         Long instanceId,
                         Long projectId,
                         Set<String> fieldList,
                         boolean allFieldCheck,
                         Set<Long> memberFieldIds) {
        super(source);
        this.component = component;
        this.event = event;
        this.instanceId = instanceId;
        this.projectId = projectId;
        this.fieldList = fieldList;
        this.allFieldCheck = allFieldCheck;
        this.memberFieldIds = memberFieldIds;
    }
    @ApiModelProperty(value = "模块")
    private String component;
    @ApiModelProperty(value = "事件")
    private String event;
    @ApiModelProperty(value = "实例id")
    private Long instanceId;
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "字段")
    private Set<String> fieldList;
    @ApiModelProperty(value = "是否全字段检查")
    private Boolean allFieldCheck = false;
    @ApiModelProperty(value = "成员字段id")
    private Set<Long> memberFieldIds;

    public Set<Long> getMemberFieldIds() {
        return memberFieldIds;
    }

    public void setMemberFieldIds(Set<Long> memberFieldIds) {
        this.memberFieldIds = memberFieldIds;
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    public Boolean getAllFieldCheck() {
        return allFieldCheck;
    }

    public void setAllFieldCheck(Boolean allFieldCheck) {
        this.allFieldCheck = allFieldCheck;
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Set<String> getFieldList() {
        return fieldList;
    }

    public void setFieldList(Set<String> fieldList) {
        this.fieldList = fieldList;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("{");
        sb.append("\"event\":\"")
                .append(event).append('\"')
                .append(",\"instanceId\":")
                .append(instanceId)
                .append(",\"projectId\":")
                .append(projectId)
                .append(",\"fieldList\":")
                .append(fieldList)
                .append(",\"allFieldCheck\":")
                .append(allFieldCheck)
                .append(",\"memberFieldIds\":")
                .append(memberFieldIds)
                .append('}');
        return sb.toString();
    }
}
