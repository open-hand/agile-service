package io.choerodon.agile.infra.annotation;

import java.lang.annotation.*;

/**
 * 页面规则消息通知
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午2:33
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface RuleNotice {
    
    String value() default "ISSUE";

    String event();

    String fieldListName() default "";
    
    String instanceIdNameInReturn() default "id";
}
