package io.choerodon.agile.app.listener;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.JavaType;
import com.google.common.base.CaseFormat;
import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.NoticeEventVO;
import io.choerodon.agile.api.vo.RuleExpressVO;
import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.ConfigurationRuleReceiverDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.ConfigurationRule;
import io.choerodon.agile.infra.enums.RuleNoticeEvent;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.mapper.ConfigurationRuleReceiverMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.CommonMapperUtil;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.domain.AuditDomain;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/9 上午9:59
 */
@Component
public class IssueNoticeListener {

    public static final String ISSUE = "ISSUE";
    public static final String BACKLOG = "BACKLOG";

    public static final Logger log = LoggerFactory.getLogger(IssueNoticeListener.class);

    @Autowired
    private ConfigurationRuleMapper configurationRuleMapper;
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private ConfigurationRuleService configurationRuleService;
    @Autowired
    private StatusNoticeSettingService statusNoticeSettingService;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ConfigurationRuleReceiverMapper configurationRuleReceiverMapper;

    @Async
    @TransactionalEventListener(NoticeEventVO.class)
    public void issueNoticeDetection(NoticeEventVO noticeEvent){
        if (checkEvent(noticeEvent)){
            return;
        }
        Long projectId = noticeEvent.getProjectId();
        Set<String> fieldList = noticeEvent.getFieldList();
        String event = noticeEvent.getEvent();
        boolean allFieldCheck = BooleanUtils.isTrue(noticeEvent.getAllFieldCheck());
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(noticeEvent.getInstanceId());
        ConfigurationRuleVO rule = new ConfigurationRuleVO();
        rule.setProjectId(projectId);
        rule.setIssueTypes(Collections.singletonList(issueDTO.getTypeCode()));
        // 筛选出检测更新字段的规则
        List<ConfigurationRuleVO> ruleVOList = processRule(configurationRuleMapper.selectByProjectId(rule), fieldList, allFieldCheck);
        // 检查issue是否符合页面规则条件
        Map<String, Long> map = CollectionUtils.isEmpty(ruleVOList) ? new HashMap<>() :
                configurationRuleMapper.selectByRuleList(issueDTO.getIssueId(), projectId, ruleVOList);
        // 获取所有符合的ruleId
        List<Long> ruleIdList = Optional.ofNullable(map).orElse(new HashMap<>())
                .values().stream().filter(Objects::nonNull).collect(Collectors.toList());
        // 组装符合条件的页面规则messageSender
        List<MessageSender> ruleSenderList = generateRuleSender(event, projectId, ruleIdList, issueDTO, fieldList);
        // 合并消息通知并发送
        mergeNotice(ruleSenderList);
        // 更改页面规则对应的处理人
        changeProcesser(issueDTO, ruleIdList, ruleVOList);
    }

    /**
     * 修改经办人
     * @param issueDTO issueDTO
     * @param ruleIdList 符合条件的规则id
     * @param ruleVOList 全部的规则
     */
    private void changeProcesser(IssueDTO issueDTO, List<Long> ruleIdList, List<ConfigurationRuleVO> ruleVOList) {
        if (CollectionUtils.isEmpty(ruleIdList)){
            return;
        }
        ConfigurationRuleVO rule = ruleVOList.stream()
                .filter(ruleVO -> ruleIdList.contains(ruleVO.getId()))
                .max(Comparator.comparing(AuditDomain::getCreationDate))
                .orElse(new ConfigurationRuleVO());
        List<ConfigurationRuleReceiverDTO> receiverList =
                configurationRuleReceiverMapper.selectReceiver(Collections.singletonList(rule.getId()),
                Collections.singletonList(ConfigurationRuleReceiverDTO.TYPE_PROCESSER));
        ConfigurationRuleReceiverDTO receiver = receiverList.stream().findFirst().orElse(null);
        if (Objects.isNull(receiver)){
            return;
        }
        issueDTO.setAssigneeId(receiver.getId());
        issueMapper.updateOptional(issueDTO, "assigneeId");
    }

    private boolean checkEvent(NoticeEventVO noticeEvent) {
        if (Objects.equals(ISSUE, noticeEvent.getSource())){
            return false;
        }
        log.debug(noticeEvent.toString());
        if (Objects.isNull(noticeEvent.getEvent())){
            return false;
        }
        if (Objects.isNull(noticeEvent.getInstanceId())){
            return false;
        }
        if (Objects.isNull(noticeEvent.getProjectId())){
            return false;
        }
        return true;
    }

    /**
     * 筛选出检测更新字段受页面规则限制的规则
     * @param sourceList 页面规则
     * @param fieldList 更新字段
     * @param allFieldCheck 是否是全字段检测
     * @return 仅对更新字段检测的规则集合
     */
    private List<ConfigurationRuleVO> processRule(List<ConfigurationRuleVO> sourceList, Set<String> fieldList, boolean allFieldCheck) {
        List<ConfigurationRuleVO> ruleList = new ArrayList<>(sourceList);
        JavaType javaType = CommonMapperUtil.getTypeFactory().constructParametricType(List.class, RuleExpressVO.class);
        for (ConfigurationRuleVO ruleVO : ruleList) {
            ruleVO.setExpressList(CommonMapperUtil.readValue(ruleVO.getExpressFormat(), javaType));
            ruleVO.setSqlQuery(configurationRuleService.generateSqlQuery(ruleVO));
        }
        if (allFieldCheck){
            // 之后消息检测需要用到
            fieldList.add("statusId");
            fieldList.add("asigneeId");
            return ruleList;
        }
        if (CollectionUtils.isEmpty(fieldList)){
            return Collections.emptyList();
        }
        return ruleList.stream()
                .filter(rule -> rule.getExpressList().stream()
                        .anyMatch(express -> fieldList.contains(fieldCode2Field(express.getFieldCode()))))
                .collect(Collectors.toList());
    }
    
    private String fieldCode2Field(String fieldCode){
        String field = ConfigurationRule.FieldTableMapping.matches(fieldCode).getField();
        if (Objects.isNull(field)){
            return CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, field);
        }
        return fieldCode;
    }

    private List<MessageSender> generateRuleSender(String event,Long projectId,List<Long> ruleIdList,
                                                   IssueDTO issue, Set<String> fieldList) {
        Map<Long, ConfigurationRuleVO> map = configurationRuleService.selectRuleALLReceiver(ruleIdList);
        // 生成需要合并的messageSenderList
        return Arrays.stream(RuleNoticeEvent.getMsgCode(event))
                .map(code ->getSenderList(generatedSenderByCode(code, projectId, issue, fieldList), map))
                .flatMap(Collection::stream).collect(Collectors.toList());
    }

    private Function<SendMsgUtil, MessageSender> generatedSenderByCode(String messageCode, Long projectId,
                                                                       IssueDTO issue, Set<String> fieldList){
        Function<SendMsgUtil, MessageSender> func = null;
        switch (messageCode){
            case RuleNoticeEvent.ISSUECREATE:
                func = msgUtil -> msgUtil.generateIssueCreatesender(projectId, issue);
                break;
            case RuleNoticeEvent.ISSUEASSIGNEE:
                func = msgUtil -> msgUtil.generateIssueAsigneeSender(projectId, fieldList, issue);
                break;
            case RuleNoticeEvent.ISSUESOLVE:
                func = msgUtil -> msgUtil.generateIssueResolvSender(projectId, fieldList, issue);
                break;
            case RuleNoticeEvent.ISSUECHANGESTATUS:
                StatusNoticeSettingVO settingVO = statusNoticeSettingService.selectNoticeUserAndType(projectId, issue.getIssueId());
                func = msgUtil -> msgUtil.generateNoticeIssueStatusSender(projectId, settingVO.getUserIdList(),
                        new ArrayList<>(settingVO.getUserTypeList()), issue, DetailsHelper.getUserDetails(), fieldList);
                break;
            default:
                break;
        }
        return func;
    }


    private List<MessageSender> getSenderList(Function<SendMsgUtil, MessageSender> func, Map<Long, ConfigurationRuleVO> map) {
        List<MessageSender> list = new ArrayList<>();
        MessageSender sourceSender = func.apply(sendMsgUtil);
        if (Objects.isNull(sourceSender)){
            return list;
        }
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
                new ArrayList<>(messageSenderList.stream().collect(Collectors.toMap(
                        t -> new MultiKey(t.getTenantId(), t.getMessageCode(),
                                Optional.ofNullable(t.getTypeCodeList()).map(HashSet::new).orElse(new HashSet<>())),
                        Function.identity(),
                        (k1, k2) -> {
                            if (CollectionUtils.isNotEmpty(k2.getCcList())){
                                List<String> ccList = Optional.ofNullable(k1.getCcList()).orElse(new ArrayList<>());
                                ccList.addAll(k2.getCcList());
                                k1.setCcList(ccList);
                            }
                            if (CollectionUtils.isNotEmpty(k2.getReceiverAddressList())){
                                List<Receiver> receiverList = Optional.ofNullable(k1.getReceiverAddressList()).orElse(new ArrayList<>());
                                receiverList.addAll(k2.getReceiverAddressList());
                                k1.setReceiverAddressList(receiverList);
                            }
                            return k1;
                        })).values());
        log.info("merge sender: before: [{}], after: [{}]", messageSenderList.size(), list.size());
        for (MessageSender messageSender : list) {
            messageClient.async().sendMessage(messageSender);
        }
    }
}
