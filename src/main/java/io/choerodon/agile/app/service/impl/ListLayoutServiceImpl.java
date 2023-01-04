package io.choerodon.agile.app.service.impl;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.api.vo.ListLayoutColumnRelVO;
import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.app.service.ListLayoutService;
import io.choerodon.agile.infra.dto.IssuePersonalSortDTO;
import io.choerodon.agile.infra.dto.ListLayoutColumnRelDTO;
import io.choerodon.agile.infra.dto.ListLayoutDTO;
import io.choerodon.agile.infra.enums.IssueFieldMapping;
import io.choerodon.agile.infra.mapper.IssuePersonalSortMapper;
import io.choerodon.agile.infra.mapper.ListLayoutColumnRelMapper;
import io.choerodon.agile.infra.mapper.ListLayoutMapper;
import io.choerodon.agile.infra.utils.UserDetailsUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:20
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ListLayoutServiceImpl implements ListLayoutService {
    @Autowired
    private ListLayoutMapper listLayoutMapper;
    @Autowired
    private ListLayoutColumnRelMapper listLayoutColumnRelMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssuePersonalSortMapper issuePersonalSortMapper;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public ListLayoutVO save(Long organizationId, Long projectId, ListLayoutVO listLayoutVO) {
        if (ObjectUtils.isEmpty(listLayoutVO.getApplyType())) {
            throw new CommonException("error.list.layout.apply.type.null");
        }
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ListLayoutDTO layoutDTO = new ListLayoutDTO(listLayoutVO.getApplyType(), userId, projectId, organizationId);
        List<ListLayoutDTO> layoutDTOS = listLayoutMapper.select(layoutDTO);
        if (CollectionUtils.isEmpty(layoutDTOS)) {
            baseInsert(layoutDTO);
        } else {
            layoutDTO = layoutDTOS.get(0);
        }
        saveColumnRel(organizationId, projectId, layoutDTO.getId(), listLayoutVO.getListLayoutColumnRelVOS());
        return queryByApplyType(organizationId, projectId, listLayoutVO.getApplyType());
    }

    private void saveColumnRel(Long organizationId, Long projectId, Long layoutId, List<ListLayoutColumnRelVO> listLayoutColumnRelVOS) {
        if(listLayoutColumnRelVOS == null) {
            listLayoutColumnRelVOS = Collections.emptyList();
        }
        listLayoutColumnRelVOS = ListLayoutColumnRelVO.distinct(listLayoutColumnRelVOS);
        ListLayoutColumnRelDTO listLayoutColumnRelDTO = new ListLayoutColumnRelDTO();
        listLayoutColumnRelDTO.setProjectId(projectId);
        listLayoutColumnRelDTO.setOrganizationId(organizationId);
        listLayoutColumnRelDTO.setLayoutId(layoutId);
        List<ListLayoutColumnRelDTO> layoutColumnRelInDb = listLayoutColumnRelMapper.select(listLayoutColumnRelDTO);
        if (projectId != null && projectId != 0L) {
            //项目层
            deleteIssuePersonalSortIfExisted(layoutColumnRelInDb, listLayoutColumnRelVOS, projectId, organizationId);
        }
        if (!CollectionUtils.isEmpty(layoutColumnRelInDb)) {
            listLayoutColumnRelMapper.delete(listLayoutColumnRelDTO);
        }
        for (ListLayoutColumnRelVO listLayoutColumnRel : listLayoutColumnRelVOS) {
            ListLayoutColumnRelDTO layoutColumnRelDTO = modelMapper.map(listLayoutColumnRel, ListLayoutColumnRelDTO.class);
            layoutColumnRelDTO.setOrganizationId(organizationId);
            layoutColumnRelDTO.setLayoutId(layoutId);
            layoutColumnRelDTO.setProjectId(projectId);
            if (listLayoutColumnRelMapper.insertSelective(layoutColumnRelDTO) != 1) {
                throw new CommonException("error.list.layout.column.rel.insert");
            }
        }
    }

    private void deleteIssuePersonalSortIfExisted(List<ListLayoutColumnRelDTO> existedLayoutColumnRelList,
                                                  List<ListLayoutColumnRelVO> inputLayoutColumnRelList,
                                                  Long projectId,
                                                  Long organizationId) {
        Set<String> inputDisplayCodes =
                inputLayoutColumnRelList.stream()
                        .filter(rel -> Boolean.TRUE.equals(rel.getDisplay()))
                        .map(ListLayoutColumnRelVO::getColumnCode)
                        .collect(Collectors.toSet());
        Set<String> deleteCodes = new HashSet<>();
        for (ListLayoutColumnRelDTO existedRel : existedLayoutColumnRelList) {
            String columnCode = existedRel.getColumnCode();
            boolean display = Boolean.TRUE.equals(existedRel.getDisplay());
            if (!inputDisplayCodes.contains(columnCode) && display) {
                deleteCodes.add(columnCode);
            }
        }
        String projectCodePrefix = "pro_";
        String organizationCodePrefix = "org_";
        String customFieldSortPrefix = "foundation.";
        if (CollectionUtils.isEmpty(deleteCodes)) {
            return;
        }
        IssuePersonalSortDTO dto = new IssuePersonalSortDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        dto.setUserId(UserDetailsUtil.getCurrentUserId());
        dto.setBusinessType("gantt");
        List<IssuePersonalSortDTO> sorts = issuePersonalSortMapper.select(dto);
        if (CollectionUtils.isEmpty(sorts)) {
            return;
        }
        IssuePersonalSortDTO sort = sorts.get(0);
        String sortJson = sort.getSortJson();
        if (StringUtils.isBlank(sortJson)) {
            return;
        }
        Map<String, IssuePersonalSortVO> sortMap = new LinkedHashMap<>();
        try {
            List<IssuePersonalSortVO> issuePersonalSorts =
                    objectMapper.readValue(sortJson, new TypeReference<List<IssuePersonalSortVO>>() {});
            for (IssuePersonalSortVO issuePersonalSort : issuePersonalSorts) {
                sortMap.put(issuePersonalSort.getProperty(), issuePersonalSort);
            }
        } catch (IOException e) {
            throw new CommonException("error.gantt.sortJson.deserialization", e);
        }
        for (String code : deleteCodes) {
            String property;
            if (code.startsWith(projectCodePrefix) || code.startsWith(organizationCodePrefix)) {
                property = customFieldSortPrefix + code;
            } else {
                property = IssueFieldMapping.getSortFieldByCode(code);
                if (property == null) {
                    property = code;
                }
            }
            sortMap.remove(property);
        }
        List<IssuePersonalSortVO> sortList = new ArrayList<>(sortMap.values());
        try {
            sortJson = objectMapper.writeValueAsString(sortList);
            sort.setSortJson(sortJson);
            if (issuePersonalSortMapper.updateByPrimaryKey(sort) != 1) {
                throw new CommonException("error.gantt.sort.save");
            }
        } catch (JsonProcessingException e) {
            throw new CommonException("error.gantt.sortJson.serialization", e);
        }
    }

    private ListLayoutDTO baseInsert(ListLayoutDTO layoutDTO) {
        if (listLayoutMapper.insertSelective(layoutDTO) != 1) {
            throw new CommonException("error.list.layout.insert");
        }
        return listLayoutMapper.selectByPrimaryKey(layoutDTO.getId());
    }

    @Override
    public ListLayoutVO queryByApplyType(Long organizationId, Long projectId, String applyType) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ListLayoutDTO listLayoutDTO = new ListLayoutDTO(applyType, userId, projectId, organizationId);
        List<ListLayoutDTO> listLayoutDTOS = listLayoutMapper.select(listLayoutDTO);
        if (CollectionUtils.isEmpty(listLayoutDTOS)) {
            return null;
        }
        ListLayoutDTO layoutDTO = listLayoutDTOS.get(0);
        ListLayoutVO layoutVO = modelMapper.map(layoutDTO, ListLayoutVO.class);
        ListLayoutColumnRelDTO listLayoutColumnRelDTO = new ListLayoutColumnRelDTO(layoutVO.getId(), projectId, organizationId);
        List<ListLayoutColumnRelDTO> list = listLayoutColumnRelMapper.select(listLayoutColumnRelDTO);
        if (!CollectionUtils.isEmpty(list)) {
            layoutVO.setListLayoutColumnRelVOS(modelMapper.map(list, new TypeToken<List<ListLayoutColumnRelVO>>() {
            }.getType()));
        }
        return layoutVO;
    }
}
