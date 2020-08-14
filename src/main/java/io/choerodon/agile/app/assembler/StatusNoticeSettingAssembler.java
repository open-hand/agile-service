package io.choerodon.agile.app.assembler;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.infra.dto.StatusNoticeSettingDTO;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.stereotype.Component;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/14 下午3:04
 */
@Component
public class StatusNoticeSettingAssembler {

    public List<StatusNoticeSettingVO> statusNoticeDto2Vo(Long projectId, Long issueTypeId,
                                                          List<StatusNoticeSettingDTO> list) {
        Map<Long, List<StatusNoticeSettingDTO>> group =
                list.stream().collect(Collectors.groupingBy(StatusNoticeSettingDTO::getStatusId));
        return group.entrySet().stream().map(entry -> {
            StatusNoticeSettingVO settingVO = new StatusNoticeSettingVO(issueTypeId, projectId, entry.getKey());
            entry.getValue().forEach(item -> settingVO.addUserWithNotice(item.getUserType(), item.getUserId()));
            settingVO.setNoticeTypeList(Stream.of(StringUtils.split(entry.getValue().stream().map(StatusNoticeSettingDTO::getNoticeType)
                    .findFirst().orElse(""), BaseConstants.Symbol.COMMA)).collect(Collectors.toList()));
            return settingVO;
        }).collect(Collectors.toList());
    }
}
