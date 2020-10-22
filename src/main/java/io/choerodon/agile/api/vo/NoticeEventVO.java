package io.choerodon.agile.api.vo;

import java.util.List;

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

    public NoticeEventVO(Object source, String event, Long instanceId, Long projectId,
                         List<String> fieldList) {
        super(source);
        this.event = event;
        this.instanceId = instanceId;
        this.projectId = projectId;
        this.fieldList = fieldList;
    }

    private String event;
    private Long instanceId;
    private Long projectId;
    private List<String> fieldList;

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

    public List<String> getFieldList() {
        return fieldList;
    }

    public void setFieldList(List<String> fieldList) {
        this.fieldList = fieldList;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("{");
        sb.append("\"event\":\"")
                .append(event).append('\"');
        sb.append(",\"instanceId\":")
                .append(instanceId);
        sb.append(",\"projectId\":")
                .append(projectId);
        sb.append(",\"fieldList\":")
                .append(fieldList);
        sb.append('}');
        return sb.toString();
    }
}
