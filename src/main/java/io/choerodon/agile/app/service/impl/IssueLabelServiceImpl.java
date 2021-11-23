package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.IssueLabelVO;
import io.choerodon.agile.app.service.IssueLabelService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldExtendDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldExtendMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.agile.infra.dto.IssueLabelDTO;
import io.choerodon.agile.infra.mapper.IssueLabelMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 敏捷开发Issue标签
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:04:00
 */
@Service
public class IssueLabelServiceImpl implements IssueLabelService {

    private static final String INSERT_ERROR = "error.IssueLabel.insert";
    private static final String AGILE = "Agile:";
    private static final String LABEL = "label";
    private static final String PIE_CHART = AGILE + "PieChart";

    @Autowired
    private IssueLabelMapper issueLabelMapper;

    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;

    @Override
    public List<IssueLabelVO> listIssueLabel(Long projectId) {
        IssueLabelDTO issueLabelDTO = new IssueLabelDTO();
        issueLabelDTO.setProjectId(projectId);
        return modelMapper.map(issueLabelMapper.select(issueLabelDTO), new TypeToken<List<IssueLabelVO>>() {
        }.getType());
    }

    @Override
    public IssueLabelDTO createBase(IssueLabelDTO issueLabelDTO) {
        String name = issueLabelDTO.getLabelName();
        if (name.length() > 20) {
            throw new CommonException("error.label.name.length");
        }
        if (issueLabelMapper.insert(issueLabelDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + issueLabelDTO.getProjectId() + ':' + LABEL + "*"});
        return issueLabelMapper.selectByPrimaryKey(issueLabelDTO.getLabelId());
    }

    @Override
    public int labelGarbageCollection(Long projectId) {
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + projectId + ':' + LABEL + "*"});
        Set<Long> labelIdsOfDefaultValue = getLabelIdsOfDefaultValue(projectId);
        return issueLabelMapper.labelGarbageCollection(projectId, labelIdsOfDefaultValue);
    }

    /**
     * 获取被设为默认值的标签ids
     *
     * @param projectId
     * @return
     */
    private Set<Long> getLabelIdsOfDefaultValue(Long projectId) {
        ObjectSchemeFieldDTO fieldDTO = objectSchemeFieldService.getObjectSchemeFieldDTO(FieldCode.LABEL);

        Set<Long> labelIdsOfDefaultValue = new HashSet<>();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, fieldDTO.getId(), projectId, null);

        objectSchemeFieldExtendList.forEach(f -> {
            String defaultValue = f.getDefaultValue();
            if (!Objects.isNull(defaultValue) && !Objects.equals(defaultValue, "")) {
                Set<Long> ids = Arrays.stream(defaultValue.split(",")).map(s -> Long.parseLong(s.trim())).collect(Collectors.toSet());
                labelIdsOfDefaultValue.addAll(ids);
            }
        });
        return labelIdsOfDefaultValue;
    }
}