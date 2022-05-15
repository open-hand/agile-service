package io.choerodon.agile.api.vo.business;

import io.swagger.annotations.ApiModelProperty;
import org.springframework.context.ApplicationEvent;

import java.util.List;

/**
 * @author superlee
 * @since 2021-08-31
 */
public class NoticeBatchEventVO extends ApplicationEvent {
    /**
     * Create a new ApplicationEvent.
     *
     * @param source the object on which the event initially occurred (never {@code null})
     */
    public NoticeBatchEventVO(Object source) {
        super(source);
    }

    public NoticeBatchEventVO(Object source,
                              List<TriggerCarrierVO> triggerCarriers,
                              String component,
                              String event) {
        super(source);
        this.triggerCarriers = triggerCarriers;
        this.component = component;
        this.event = event;
    }

    @ApiModelProperty("触发器负载器")
    private List<TriggerCarrierVO> triggerCarriers;
    @ApiModelProperty("模块")
    private String component;
    @ApiModelProperty("事件")
    private String event;

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    public List<TriggerCarrierVO> getTriggerCarriers() {
        return triggerCarriers;
    }

    public void setTriggerCarriers(List<TriggerCarrierVO> triggerCarriers) {
        this.triggerCarriers = triggerCarriers;
    }
}
