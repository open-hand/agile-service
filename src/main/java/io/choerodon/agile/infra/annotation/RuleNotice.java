package io.choerodon.agile.infra.annotation;

import java.lang.annotation.*;

import io.choerodon.agile.api.vo.NoticeEventVO;

/**
 * 页面规则消息通知
 * 可监听{@link NoticeEventVO}事件实现消息处理
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午2:33
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface RuleNotice {

    String LABEL_ID = "labelId";
    String VERSION_ID = "versionId";
    String COMPONENT_ID = "componentId";

    /**
     * 通知检测所使用的模块
     */
    String value() default "ISSUE";

    /**
     * 触发事件。事件可能与多个消息关连。
     * 需要手动指定事件关联的消息代码。可参照{@link io.choerodon.agile.infra.enums.RuleNoticeEvent#getMsgCode(java.lang.String)}
     */
    String event();

    /**
     * 从方法参数中获取要更新的字段list，与{@link RuleNotice#fieldList()}互斥。
     * 若同时存在，则优先取{@link RuleNotice#fieldList()}
     * @return 字段list
     */
    String fieldListName() default "";

    /**
     * 从返回值中获取实例instanceId的字段名称
     */
    String instanceId() default "id";

    /**
     * 实例id所在位置，默认在返回值中
     * @return result 或 arg
     */
    String idPosition() default "result";

    /**
     * 手动指定要更新的字段list，优先取值
     * @return 字段list
     */
    String[] fieldList() default {};

    /**
     * 是否是全字段检测
     * 此配置开启时, 与fieldList和fieldListName互斥。推荐创建时使用，默认关闭
     * @return 默认false
     */
    boolean allFieldCheck() default false;

    /**
     * 标记问题创建后，设置自定义字段值的方法，用于处理触发器通知对象为人员自定义字段的情况
     *
     * @return
     */
    boolean customFieldsAfterInstanceCreate() default false;

    boolean isBatch() default false;
}
