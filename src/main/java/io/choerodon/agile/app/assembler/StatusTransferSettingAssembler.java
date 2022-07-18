package io.choerodon.agile.app.assembler;

import io.choerodon.agile.api.vo.RoleVO;
import io.choerodon.agile.api.vo.StatusTransferSettingVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.StatusTransferType;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.util.*;

/**
 * @author zhaotianxin
 * @date 2020-08-12 13:55
 */
@Component
public class StatusTransferSettingAssembler {
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;

    private static final String CUSTOM_FIELD_ORGANIZATION_PREFIX = "org_";

    public List<StatusTransferSettingVO> listDTOToVO(List<StatusTransferSettingDTO> dtos,
                                                     Map<Long, UserDTO> userMap,
                                                     Map<Long, RoleVO> roleMap,
                                                     Long organizationId) {
        List<StatusTransferSettingVO> list = modelMapper.map(dtos, new TypeToken<List<StatusTransferSettingVO>>() {
        }.getType());
        Set<String> fieldCodes = new HashSet<>();
        for (StatusTransferSettingVO statusTransferSettingVO : list) {
            String userType = statusTransferSettingVO.getUserType();
            Long userId = statusTransferSettingVO.getUserId();
            if (!StatusTransferType.ALL_TYPES.contains(userType)) {
                fieldCodes.add(userType);
            } else if (StatusTransferType.isSpecifier(userType)) {
                statusTransferSettingVO.setUser(userMap.get(userId));
            } else if (StatusTransferType.isRole(userType)) {
                statusTransferSettingVO.setRole(roleMap.get(userId));
            }
        }
        if (!fieldCodes.isEmpty()) {
            List<ObjectSchemeFieldDTO> fields = objectSchemeFieldMapper.selectFieldsByFieldCodes(organizationId, new ArrayList<>(fieldCodes));
            //keys projectId, fieldCode
            Map<Long, Map<String, String>> fieldNameMap = new HashMap<>();
            fields.forEach(x -> {
                Long projectId = x.getProjectId();
                if (ObjectUtils.isEmpty(projectId)) {
                    projectId = 0L;
                }
                Map<String, String> fieldCodeNameMap = fieldNameMap.computeIfAbsent(projectId, y -> new HashMap<>());
                fieldCodeNameMap.put(x.getCode(), x.getName());
            });
            list.forEach(statusTransferSetting -> {
                String userType = statusTransferSetting.getUserType();
                Long projectId = statusTransferSetting.getProjectId();
                if (StatusTransferType.ALL_TYPES.contains(userType)) {
                    return;
                }
                if (userType.startsWith(CUSTOM_FIELD_ORGANIZATION_PREFIX)) {
                    projectId = 0L;
                }
                if (ObjectUtils.isEmpty(projectId)) {
                    projectId = 0L;
                }
                Map<String, String> fieldCodeNameMap = fieldNameMap.getOrDefault(projectId, Collections.emptyMap());
                statusTransferSetting.setFieldName(fieldCodeNameMap.get(userType));
            });
        }
        return list;
    }
}
