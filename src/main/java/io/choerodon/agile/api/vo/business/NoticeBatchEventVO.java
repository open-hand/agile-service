package io.choerodon.agile.api.vo.business;

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
                              String component) {
        super(source);
        this.triggerCarriers = triggerCarriers;
        this.component = component;
    }

    private List<TriggerCarrierVO> triggerCarriers;

    private String component;

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
