package io.choerodon.agile.infra.aspect;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.choerodon.agile.app.service.NoticeDetectionService;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/25 下午2:34
 */
@Aspect
@Component
@Transactional(rollbackFor = Exception.class)
public class RuleNoticeAspect {
    
    public static final String ISSUE = "ISSUE";
    public static final String BACKLOG = "BACKLOG";
    
    public static final Logger log = LoggerFactory.getLogger(RuleNoticeAspect.class);
    
    @Autowired
    private NoticeDetectionService noticeDetectionService;
    @Autowired
    private IssueMapper issueMapper;
    
    @Pointcut("@annotation(io.choerodon.agile.infra.annotation.RuleNotice)")
    public void pointCut(){}
    
    @AfterReturning(value = "pointCut()", returning = "result")
    public void after(JoinPoint jp, Object result){
        MethodSignature sign = (MethodSignature) jp.getSignature();
        Method method = sign.getMethod();
        RuleNotice ruleNotice = method.getAnnotation(RuleNotice.class);
        // 这里fieldList如果没有传值，一定要为null，不可以为空集合，空集合代表存在指定字段更新但指定字段为空，后面需要根据是否为null来判断发消息
        List<String> fieldList = StringUtils.isBlank(ruleNotice.fieldListName()) ? 
                null : (List<String>) getNameAndValue(jp).get(ruleNotice.fieldListName());
        Long projectId = (Long)Reflections.getFieldValue(result, "projectId");
        log.info("rule notice detection, component: [{}], event: [{}]", ruleNotice.value(), ruleNotice.event());
        switch (ruleNotice.value()){
            case ISSUE:
                Long issueId = (Long)Reflections.getFieldValue(result, "issueId");
                IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
                noticeDetectionService.issueNoticeDetection(ruleNotice.event(), issueDTO, projectId, fieldList);
                break;
            case BACKLOG:
                Long backlogId = (Long)Reflections.getFieldValue(result, "id");
                backlogNoticeDetection(backlogId, projectId);
                break;
            default:
                break;
        }
    }

    private void backlogNoticeDetection(Long backlogId, Long projectId) {
        // TODO 
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
