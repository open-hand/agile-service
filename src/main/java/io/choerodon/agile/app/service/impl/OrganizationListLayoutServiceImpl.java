package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ListLayoutColumnRelVO;
import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.ListLayoutService;
import io.choerodon.agile.app.service.OrganizationListLayoutService;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-10-19
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class OrganizationListLayoutServiceImpl implements OrganizationListLayoutService {

    @Autowired
    private ListLayoutService listLayoutService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private RemoteIamOperator remoteIamOperator;

    @Override
    public ListLayoutVO queryByApplyType(Long organizationId, String applyType) {
        ListLayoutVO layout = listLayoutService.queryByApplyType(organizationId, 0L, applyType);
        if (ObjectUtils.isEmpty(layout)) {
            return null;
        }
        addCustomFieldProjectName(layout.getListLayoutColumnRelVOS());
        return layout;
    }

    private void addCustomFieldProjectName(List<ListLayoutColumnRelVO> listLayoutColumnRels) {
        if (ObjectUtils.isEmpty(listLayoutColumnRels)) {
            return;
        }
        Set<Long> fieldIds =
                listLayoutColumnRels
                        .stream()
                        .filter(x -> !ObjectUtils.isEmpty(x.getFieldId()))
                        .map(ListLayoutColumnRelVO::getFieldId)
                        .collect(Collectors.toSet());
        if (fieldIds.isEmpty()) {
            return;
        }
        Map<Long, Long> fieldProjectIdMap =
                objectSchemeFieldMapper.selectByIds(StringUtils.join(fieldIds, ","))
                        .stream()
                        .filter(x -> Boolean.FALSE.equals(x.getSystem()))
                        .filter(x -> !ObjectUtils.isEmpty(x.getProjectId()) && !Objects.equals(0L, x.getProjectId()))
                        .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, ObjectSchemeFieldDTO::getProjectId));
        Set<Long> projectIds = new HashSet<>(fieldProjectIdMap.values());
        if (projectIds.isEmpty()) {
            return;
        }
        List<ProjectVO> projects = remoteIamOperator.queryProjectByIds(projectIds);
        Map<Long, String> projectNameMap = projects.stream().collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getName));
        listLayoutColumnRels.forEach(column -> {
            Long fieldId = column.getFieldId();
            if (ObjectUtils.isEmpty(fieldId)) {
                return;
            }
            Long projectId = fieldProjectIdMap.get(fieldId);
            if (ObjectUtils.isEmpty(projectId)) {
                return;
            }
            String projectName = projectNameMap.get(projectId);
            if (!ObjectUtils.isEmpty(projectName)) {
                column.setFieldProjectName(projectName);
            }
        });
    }

    @Override
    public ListLayoutVO save(Long organizationId, ListLayoutVO listLayoutVO) {
        return listLayoutService.save(organizationId, 0L, listLayoutVO);
    }
}
