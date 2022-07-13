package io.choerodon.agile.app.assembler;

import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.StatusTransferSettingVO;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.StatusTransferType;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:55
 */
@Component
public class StatusTransferSettingAssembler {
    @Autowired
    private ModelMapper modelMapper;

    public List<StatusTransferSettingVO> listDTOToVO(List<StatusTransferSettingDTO> dtos,
                                                     Map<Long, UserDTO> userMap,
                                                     Map<Long, RoleVO> roleMap) {
        List<StatusTransferSettingVO> list = modelMapper.map(dtos, new TypeToken<List<StatusTransferSettingVO>>() {
        }.getType());
        for (StatusTransferSettingVO statusTransferSettingVO : list) {
            String userType = statusTransferSettingVO.getUserType();
            Long userId = statusTransferSettingVO.getUserId();
            if (ObjectUtils.isEmpty(userId)) {
                continue;
            }
            if (StatusTransferType.isSpecifier(userType)) {
                statusTransferSettingVO.setUser(userMap.get(userId));
            } else if (StatusTransferType.isRole(userType)) {
                statusTransferSettingVO.setRole(roleMap.get(userId));
            }
        }
        return list;
    }
}
