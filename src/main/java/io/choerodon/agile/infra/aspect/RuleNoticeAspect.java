package io.choerodon.agile.infra.aspect;

import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.RuleNoticeEvent;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.CodeSignature;
import org.aspectj.lang.reflect.MethodSignature;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
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
    public static final String EXIST_FLAG  = "exist";
    public static final String ISSUECREATE = "ISSUECREATE";
    public static final String ISSUEASSIGNEE = "ISSUEASSIGNEE";
    public static final String ISSUESOLVE = "ISSUESOLVE";
    public static final String ISSUECHANGESTATUS = "ISSUECHANGESTATUS";
    
    public static final Logger log = LoggerFactory.getLogger(RuleNoticeAspect.class);
    
    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    
    @Pointcut("@annotation(io.choerodon.agile.infra.annotation.RuleNotice)")
    public void pointCut(){}
    
    @AfterReturning(value = "pointCut()", returning = "result")
    public void after(JoinPoint jp, Object result){
        MethodSignature sign = (MethodSignature) jp.getSignature();
        Method method = sign.getMethod();
        RuleNotice ruleNotice = method.getAnnotation(RuleNotice.class);
        // 这里fieldList如果没有传值，一定要为null，不可以为空集合，空集合代表存在指定字段更新但指定字段为空，后面需要根据是否为null来判断发消息
        List<String> fieldList = StringUtils.isBlank(ruleNotice.fieldListName()) ? 
                null : Arrays.asList((String[])getNameAndValue(jp).get(ruleNotice.fieldListName()));
        Long projectId = (Long)Reflections.getFieldValue(result, "projectId");
        log.info("rule notice detcction, component: [{}], event: [{}]", ruleNotice.value(), ruleNotice.event());
        switch (ruleNotice.value()){
            case ISSUE:
                Long issueId = (Long)Reflections.getFieldValue(result, "issueId");
                issueNoticeDetection(ruleNotice.event(), issueId, projectId, fieldList);
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

    private void issueNoticeDetection(RuleNoticeEvent event, Long issueId, Long projectId, List<String> fieldList){
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        List<ConfigurationRuleVO> ruleVOList = configurationRuleMapper.selectByProjectId(projectId);
        if (CollectionUtils.isEmpty(ruleVOList)){
            return;
        }
        // 检查issue是否符合页面规则条件
        Map<String, Long> map = configurationRuleMapper.selectByRuleList(issueId, ruleVOList);
        // 获取所有符合的ruleId
        List<Long> ruleIdList = map.values().stream().filter(Objects::nonNull).collect(Collectors.toList());
        // 组装符合条件的页面规则messageSender
        List<MessageSender> ruleSenderList = generateRuleSender(event, projectId, ruleIdList, issueDTO, fieldList);
        // 合并消息通知
        mergeNotice(ruleSenderList);
    }

    private List<MessageSender> generateRuleSender(RuleNoticeEvent event,Long projectId,List<Long> ruleIdList, 
                                                   IssueDTO issue, List<String> fieldList) {
        Map<Long, ConfigurationRuleVO> map = configurationRuleService.selectRuleReceiverWithCc(ruleIdList);
        // 生成需要合并的messageSenderList
        return Arrays.stream(event.getMessageCodeList())
                .map(code ->getSenderList(generatedSenderByCode(code, projectId, issue, fieldList), map))
                .flatMap(Collection::stream).collect(Collectors.toList());
    }
    
    private Function<SendMsgUtil, MessageSender> generatedSenderByCode(String messageCode, Long projectId, 
                                                                       IssueDTO issue, List<String> fieldList){
        Function<SendMsgUtil, MessageSender> func = null;
        switch (messageCode){
            case ISSUECREATE: 
                func = msgUtil -> msgUtil.generateIssueCreatesender(projectId, issue);
                break;
            case ISSUEASSIGNEE:
                func = msgUtil -> msgUtil.generateIssueAsigneeSender(projectId, fieldList, issue);
                break;
            case ISSUESOLVE:
                func = msgUtil -> msgUtil.generateIssueResolvSender(projectId, fieldList, issue);
                break;
            case ISSUECHANGESTATUS:
                StatusNoticeSettingVO settingVO = statusNoticeSettingService.selectNoticeUserAndType(projectId, issue.getIssueId());
                func = msgUtil -> msgUtil.generateNoticeIssueStatusSender(projectId, settingVO.getUserIdList(), 
                        new ArrayList<>(settingVO.getUserTypeList()), issue, DetailsHelper.getUserDetails()); 
                break;
            default:
                break;
        }
        return func;
    }
    

    private List<MessageSender> getSenderList(Function<SendMsgUtil, MessageSender> func, Map<Long, ConfigurationRuleVO> map) {
        List<MessageSender> list = new ArrayList<>();
        MessageSender sourceSender = func.apply(sendMsgUtil);
        list.add(sourceSender);
        list.addAll(map.values().stream().map(rule -> generateSenderReceivetList(sourceSender, rule)).collect(Collectors.toList()));
        list.remove(null);
        return list;
    }

    private MessageSender generateSenderReceivetList(MessageSender issueCreate, ConfigurationRuleVO rule) {
        MessageSender messageSender = new MessageSender(issueCreate);
        messageSender.setReceiverAddressList(handleUserDTO2Receiver(rule.getReceiverList()));
        messageSender.setCcList(handleUserDTO2Cc(rule.getCcList()));
        return messageSender;
    }

    private List<String> handleUserDTO2Cc(List<UserDTO> ccList) {
        if (CollectionUtils.isEmpty(ccList)){
            return null;
        }
        return ccList.stream().map(UserDTO::getEmail).collect(Collectors.toList());
    }

    private List<Receiver> handleUserDTO2Receiver(List<UserDTO> receiverList) {
        if (CollectionUtils.isEmpty(receiverList)){
            return null;
        }
        return receiverList.stream().map( userDTO -> {
            Receiver receiver = new Receiver();
            receiver.setUserId(userDTO.getId());
            receiver.setEmail(userDTO.getEmail());
            receiver.setPhone(userDTO.getPhone());
            receiver.setTargetUserTenantId(userDTO.getOrganizationId());
            return receiver;
        }).collect(Collectors.toList());
    }

    private void mergeNotice(List<MessageSender> messageSenderList) {
        List<MessageSender> list =
                messageSenderList.stream().filter(distinct(MessageSenderUniqueVO::new)).collect(Collectors.toList());
        log.info("merge sender: before: [{}], after: [{}]", messageSenderList.size(), list.size());
        for (MessageSender messageSender : list) {
            messageClient.async().sendMessage(messageSender);
        }
    }
    
    private Predicate<MessageSender> distinct(Function<MessageSender, MessageSenderUniqueVO> keyExtractor){
        Map<MultiKey, MessageSenderUniqueVO> map = new ConcurrentHashMap<>();
        return t -> {
            MultiKey multiKey = new MultiKey(t.getTenantId(), t.getMessageCode(), 
                    Optional.ofNullable(t.getTypeCodeList()).map(HashSet::new).orElse(new HashSet<>()));
            MessageSenderUniqueVO value = keyExtractor.apply(t);
            MessageSenderUniqueVO exist = map.get(multiKey);
            log.debug("currend sender: [{}], isRepeat: [{}]", value, Objects.isNull(exist));
            if (Objects.isNull(exist)) {
                map.put(multiKey, value);
            }else {
                // merge receiverList and ccList
                exist.getCcList().addAll(value.getCcList());
                exist.getReceiverList().addAll(value.getReceiverList());
                map.put(multiKey, exist);
            }
            return Objects.isNull(exist);
        };
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
