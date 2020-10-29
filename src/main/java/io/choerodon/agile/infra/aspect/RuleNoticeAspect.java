package io.choerodon.agile.infra.aspect;

import java.lang.reflect.Method;
import java.util.*;

import io.choerodon.agile.api.vo.NoticeEventVO;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.core.convertor.ApplicationContextHelper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.CodeSignature;
import org.aspectj.lang.reflect.MethodSignature;
import org.hzero.core.util.Reflections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午2:34
 */
@Aspect
@Component
@Transactional(rollbackFor = Exception.class)
public class RuleNoticeAspect {
    
    public static final Logger log = LoggerFactory.getLogger(RuleNoticeAspect.class);
    
    @Pointcut("@annotation(io.choerodon.agile.infra.annotation.RuleNotice)")
    public void pointCut(){}
    
    @AfterReturning(value = "pointCut()", returning = "result")
    public void after(JoinPoint jp, Object result){
        MethodSignature sign = (MethodSignature) jp.getSignature();
        Method method = sign.getMethod();
        RuleNotice ruleNotice = method.getAnnotation(RuleNotice.class);
        Set<String> fieldList = StringUtils.isBlank(ruleNotice.fieldListName()) ?
                null : new HashSet<>(getFieldList(getMethodArg(ruleNotice, jp, ruleNotice.fieldListName())) );
        Long projectId = Optional.ofNullable(result).map(v -> (Long)Reflections.getFieldValue(v, "projectId")).orElse(null) ;
        log.info("rule notice detection, component: [{}], event: [{}]", ruleNotice.value(), ruleNotice.event());
        ApplicationContext context = ApplicationContextHelper.getContext();
        Long instanceId;
        if (StringUtils.equals(ruleNotice.idPosition(), "result")){
            instanceId = Optional.ofNullable(result).map(v -> (Long)Reflections.getFieldValue(result, ruleNotice.instanceId())).orElse(null);
        }else {
            instanceId = (Long) getMethodArg(ruleNotice, jp, ruleNotice.idPosition());
        }
        context.publishEvent(new NoticeEventVO(result, ruleNotice.value(), ruleNotice.event(), instanceId, projectId, fieldList, ruleNotice.allFieldCheck()));
    }

    /**
     * 这里fieldList如果没有传值，一定要为null，不可以为空集合，空集合代表存在指定字段更新但指定字段为空，后面需要根据是否为null来进行规则检测
     * @param ruleNotice ruleNotice
     * @param jp jp
     * @return fieldList
     */
    private Object getMethodArg(RuleNotice ruleNotice, JoinPoint jp, String fieldName) {
        List<String> fieldList = new ArrayList<>(Arrays.asList(ruleNotice.fieldList()));
        if (CollectionUtils.isNotEmpty(fieldList)){
            return new HashSet<>(fieldList);
        }
        return getNameAndValue(jp).get(ruleNotice.fieldListName());
    }

    private List<String> getFieldList(Object field) {
        if (field instanceof List){
            return (List<String>)field;
        }else {
            return Collections.singletonList((String)field);
        }
        
    }

    Map<String, Object> getNameAndValue(JoinPoint joinPoint) {
        Map<String, Object> param = new HashMap<>();
        Object[] paramValues = joinPoint.getArgs();
        String[] paramNames = ((CodeSignature) joinPoint.getSignature()).getParameterNames();
        for (int i = 0; i < paramNames.length; i++) {
            param.put(paramNames[i], paramValues[i]);
        }
        return param;
    }

}
