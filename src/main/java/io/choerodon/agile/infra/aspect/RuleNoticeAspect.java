package io.choerodon.agile.infra.aspect;

import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.MessageSenderUniqueVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections4.CollectionUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.hzero.core.util.Reflections;
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
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    
    @Pointcut("@annotation(io.choerodon.agile.infra.annotation.RuleNotice)")
    public void pointCut(){}
    
    @AfterReturning(value = "pointCut()", returning = "result")
    public void after(JoinPoint jp, Object result){
        MethodSignature sign = (MethodSignature) jp.getSignature();
        Method method = sign.getMethod();
        RuleNotice ruleNotice = method.getAnnotation(RuleNotice.class);
        Long projectId = (Long)Reflections.getFieldValue(result, "projectId");
        switch (ruleNotice.value()){
            case ISSUE:
                Long issueId = (Long)Reflections.getFieldValue(result, "issueId");
                issueNoticeDetection(issueId, projectId);
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

    private void issueNoticeDetection(Long issueId, Long projectId){
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
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
        List<MessageSender> messageSenderList = new ArrayList<>();
        // 查询收件人，抄送人，准备发消息
        // 组装符合条件的页面规则messageSender
        List<MessageSender> ruleSenderList = generateRuleSender(projectId, ruleIdList, issueDTO);
        messageSenderList.addAll(ruleSenderList);
        // issue分配
        // issue已解决
        // issue状态变更
        // 合并消息通知
        mergeNotice(messageSenderList);
    }

    private List<MessageSender> generateRuleSender(Long projectId,List<Long> ruleIdList, IssueDTO issue) {
        List<MessageSender> list = new ArrayList<>();
        Map<Long, ConfigurationRuleVO> map = configurationRuleService.selectRuleReceiverWithCc(ruleIdList);
        // issue创建
        MessageSender issueCreate = sendMsgUtil.generateIssueCreateMessageSender(projectId, issue);
        List<MessageSender> issueCreateList = map.values().stream().map(rule -> generateRuleSender(issueCreate, rule)).collect(Collectors.toList());
        list.addAll(issueCreateList);
        // issue分配
        // issue已解决
        // issue状态变更
        
        return list;
    }

    private MessageSender generateRuleSender(MessageSender issueCreate, ConfigurationRuleVO rule) {
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
        for (MessageSender messageSender : list) {
            messageClient.async().sendMessage(messageSender);
        }
    }
    
    private Predicate<MessageSender> distinct(Function<MessageSender, MessageSenderUniqueVO> keyExtractor){
        Map<MultiKey, MessageSenderUniqueVO> map = new ConcurrentHashMap<>();
        return t -> {
            MultiKey multiKey = new MultiKey(t.getTenantId(), t.getMessageCode(), new HashSet<>(t.getTypeCodeList()));
            MessageSenderUniqueVO value = keyExtractor.apply(t);
            MessageSenderUniqueVO exist = map.get(multiKey);
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
}
