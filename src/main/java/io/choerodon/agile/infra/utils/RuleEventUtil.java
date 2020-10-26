package io.choerodon.agile.infra.utils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.ConfigurationRuleVO;
import io.choerodon.agile.api.vo.NoticeEventVO;
import io.choerodon.agile.infra.dto.UserDTO;
import org.apache.commons.collections.keyvalue.MultiKey;
import org.apache.commons.collections4.CollectionUtils;
import org.hzero.boot.message.entity.MessageSender;
import org.hzero.boot.message.entity.Receiver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author jiaxu.cui@hand-china.com 2020/10/26 下午2:45
 */
public class RuleEventUtil {
    
    public static final Logger log = LoggerFactory.getLogger(RuleEventUtil.class);

    public static boolean checkEvent(NoticeEventVO noticeEvent, String source) {
        if (Objects.equals(source, noticeEvent.getComponent())){
            return false;
        }
        log.debug(noticeEvent.toString());
        return !Objects.isNull(noticeEvent.getEvent());
    }
    
    public static <T> List<MessageSender> getSenderList(T t, Function<T, MessageSender> func, Map<Long, ConfigurationRuleVO> map) {
        List<MessageSender> list = new ArrayList<>();
        MessageSender sourceSender = func.apply(t);
        if (Objects.isNull(sourceSender)){
            return list;
        }
        list.add(sourceSender);
        list.addAll(map.values().stream().map(rule -> generateSenderReceivetList(sourceSender, rule)).collect(Collectors.toList()));
        list.remove(null);
        return list;
    }

    public static List<MessageSender> mergeNotice(List<MessageSender> messageSenderList) {
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
        return list;
    }

    private static List<String> handleUserDTO2Cc(List<UserDTO> ccList) {
        if (CollectionUtils.isEmpty(ccList)){
            return null;
        }
        return ccList.stream().map(UserDTO::getEmail).collect(Collectors.toList());
    }

    private static List<Receiver> handleUserDTO2Receiver(List<UserDTO> receiverList) {
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

    private static MessageSender generateSenderReceivetList(MessageSender issueCreate, ConfigurationRuleVO rule) {
        MessageSender messageSender = new MessageSender(issueCreate);
        messageSender.setReceiverAddressList(RuleEventUtil.handleUserDTO2Receiver(rule.getReceiverList()));
        messageSender.setCcList(RuleEventUtil.handleUserDTO2Cc(rule.getCcList()));
        return messageSender;
    }
}
