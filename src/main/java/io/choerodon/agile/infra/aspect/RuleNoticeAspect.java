package io.choerodon.agile.infra.aspect;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.MessageSenderUniqueVO;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.ss.formula.functions.T;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.hzero.boot.message.entity.MessageSender;
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
    public static final String EXIST_FLAG  = "exist";
    
    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    private IssueMapper issueMapper;
    
    @Pointcut("@annotation(io.choerodon.agile.infra.annotation.RuleNotice)")
    public void pointCut(){}
    
    @AfterReturning(value = "pointCut()", returning = "result")
    public void after(JoinPoint jp, Object result){
        MethodSignature sign = (MethodSignature) jp.getSignature();
        Method method = sign.getMethod();
        RuleNotice ruleNotice = method.getAnnotation(RuleNotice.class);
        switch (ruleNotice.value()){
            case ISSUE:
//                issueNoticeDetection(result.getIssueId(), result.getProjectId());
                break;
            case BACKLOG:
//                backlogNoticeDetection(result.getIssueId(), result.getProjectId());
                break;
            default:
                break;
        }
    }
    
    private void issueNoticeDetection(Long issueId, Long projectId){
        List<ConfigurationRuleVO> ruleVOList = configurationRuleMapper.selectByProjectId(projectId);
        if (CollectionUtils.isEmpty(ruleVOList)){
            return;
        }
        // 检查issue是否符合页面规则条件
        Map<String, Long> map = configurationRuleMapper.selectByRuleList(issueId, ruleVOList);
        if (!Objects.equals(map.get(EXIST_FLAG), 1L)){
            return;
        }
        // 获取所有符合的ruleId
        map.remove(EXIST_FLAG);
        List<Long> ruleIdList = map.values().stream().filter(Objects::nonNull).collect(Collectors.toList());
        // 查询收件人，抄送人，准备发消息
        // FIXME 整合messageSender
        List<MessageSender> messageSenderList = new ArrayList<>();
        // 合并消息通知
        mergeNotice(messageSenderList);
    }

    private void mergeNotice(List<MessageSender> messageSenderList) {
        messageSenderList.stream().filter(distinct((MessageSenderUniqueVO::new))).collect(Collectors.toList());
    }
    
    private <T> Predicate<T> distinct(Function<? super T, ?> keyExtractor){
        Map<Object, Boolean> map = new ConcurrentHashMap<>();
        return t -> map.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }
}
