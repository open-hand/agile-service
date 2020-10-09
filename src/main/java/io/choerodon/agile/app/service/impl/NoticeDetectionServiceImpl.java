package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.MessageSenderUniqueVO;
import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.ConfigurationRuleService;
import io.choerodon.agile.app.service.NoticeDetectionService;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.RuleNoticeEvent;
import io.choerodon.agile.infra.mapper.ConfigurationRuleMapper;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections4.CollectionUtils;
import org.hzero.boot.message.MessageClient;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/9 上午9:59
 */
@Service
public class NoticeDetectionServiceImpl implements NoticeDetectionService {

    public static final String ISSUECREATE = "ISSUECREATE";
    public static final String ISSUEASSIGNEE = "ISSUEASSIGNEE";
    public static final String ISSUESOLVE = "ISSUESOLVE";
    public static final String ISSUECHANGESTATUS = "ISSUECHANGESTATUS";

    public static final Logger log = LoggerFactory.getLogger(NoticeDetectionServiceImpl.class);

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

    @Async
    @Override
    public void issueNoticeDetection(RuleNoticeEvent event, IssueDTO issueDTO, Long projectId, List<String> fieldList){
        List<ConfigurationRuleVO> ruleVOList = configurationRuleMapper.selectByProjectId(projectId);
        if (CollectionUtils.isEmpty(ruleVOList)){
            return;
        }
        // 检查issue是否符合页面规则条件
        Map<String, Long> map = configurationRuleMapper.selectByRuleList(issueDTO.getIssueId(), ruleVOList);
        // 获取所有符合的ruleId
        List<Long> ruleIdList = Optional.ofNullable(map).orElse(new HashMap<>())
                .values().stream().filter(Objects::nonNull).collect(Collectors.toList());
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

}
