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

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.api.vo.ListLayoutColumnRelVO;
import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.app.service.ListLayoutService;
import io.choerodon.agile.domain.repository.PersonalSortRepository;
import io.choerodon.agile.infra.dto.IssuePersonalSortDTO;
import io.choerodon.agile.infra.dto.ListLayoutColumnRelDTO;
import io.choerodon.agile.infra.dto.ListLayoutDTO;
import io.choerodon.agile.infra.enums.IssueFieldMapping;
import io.choerodon.agile.infra.mapper.ListLayoutColumnRelMapper;
import io.choerodon.agile.infra.mapper.ListLayoutMapper;
import io.choerodon.agile.infra.utils.UserDetailsUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.lock.annotation.Lock;
import org.hzero.lock.enums.LockType;

/**
 * @author zhaotianxin 2021-05-07 14:20
 */
@Service
public class ListLayoutServiceImpl implements ListLayoutService {
    @Autowired
    private ListLayoutMapper listLayoutMapper;
    @Autowired
    private ListLayoutColumnRelMapper listLayoutColumnRelMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PersonalSortRepository personalSortRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    @Transactional(rollbackFor = Exception.class)
    @Lock(
            name = "'ListLayout-save-lock.' + #organizationId + '-' + #projectId + '-' + T(io.choerodon.core.oauth.DetailsHelper).getUserDetails().getUserId() + '-' + #listLayoutVO.applyType",
            nameIsSpel = true,
            lockType = LockType.WRITE,
            waitTime = 30L,
            leaseTime = -1L
    )
    public ListLayoutVO save(Long organizationId, Long projectId, ListLayoutVO listLayoutVO) {
        final String applyType = listLayoutVO.getApplyType();
        if (StringUtils.isBlank(applyType)) {
            throw new CommonException("error.list.layout.apply.type.null");
        }
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ListLayoutDTO layout = new ListLayoutDTO(applyType, userId, projectId, organizationId);
        ListLayoutDTO layoutInDb = listLayoutMapper.selectOne(layout);
        if (layoutInDb == null) {
            baseInsert(layout);
        } else {
            layout = layoutInDb;
        }
        saveColumnRel(organizationId, projectId, layout.getId(), applyType, listLayoutVO.getListLayoutColumnRelVOS());
        return queryByApplyType(organizationId, projectId, applyType);
    }

    private void saveColumnRel(Long organizationId, Long projectId, Long layoutId, String applyType, List<ListLayoutColumnRelVO> listLayoutColumnRelVOS) {
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
            deleteIssuePersonalSortIfExisted(layoutColumnRelInDb, listLayoutColumnRelVOS, projectId, organizationId, applyType);
        }
        if (!CollectionUtils.isEmpty(layoutColumnRelInDb)) {
            listLayoutColumnRelMapper.delete(listLayoutColumnRelDTO);
        }
        for (ListLayoutColumnRelVO listLayoutColumnRel : listLayoutColumnRelVOS) {
            ListLayoutColumnRelDTO layoutColumnRelDTO = modelMapper.map(listLayoutColumnRel, ListLayoutColumnRelDTO.class);
            layoutColumnRelDTO.setOrganizationId(organizationId);
            layoutColumnRelDTO.setLayoutId(layoutId);
            layoutColumnRelDTO.setProjectId(projectId);
            layoutColumnRelDTO.setId(null);
            if (listLayoutColumnRelMapper.insertSelective(layoutColumnRelDTO) != 1) {
                throw new CommonException("error.list.layout.column.rel.insert");
            }
        }
    }

    private void deleteIssuePersonalSortIfExisted(List<ListLayoutColumnRelDTO> existedLayoutColumnRelList,
                                                  List<ListLayoutColumnRelVO> inputLayoutColumnRelList,
                                                  Long projectId,
                                                  Long organizationId,
                                                  String applyType) {
        Set<String> inputDisplayCodes = inputLayoutColumnRelList.stream()
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
        if (CollectionUtils.isEmpty(deleteCodes)) {
            return;
        }

        IssuePersonalSortDTO personalSort = new IssuePersonalSortDTO();
        personalSort.setOrganizationId(organizationId);
        personalSort.setProjectId(projectId);
        personalSort.setUserId(UserDetailsUtil.getCurrentUserId());
        personalSort.setBusinessType(applyType);
        personalSort = this.personalSortRepository.selectOne(personalSort);
        if (personalSort == null) {
            return;
        }
        String sortJson = personalSort.getSortJson();
        if (StringUtils.isBlank(sortJson)) {
            return;
        }
        Map<String, IssuePersonalSortVO> sortMap = new LinkedHashMap<>();
        List<IssuePersonalSortVO> issuePersonalSorts;
        try {
            issuePersonalSorts = objectMapper.readValue(sortJson, new TypeReference<List<IssuePersonalSortVO>>() {});
        } catch (IOException e) {
            throw new CommonException("error.gantt.sortJson.deserialization", e);
        }
        for (IssuePersonalSortVO issuePersonalSort : issuePersonalSorts) {
            sortMap.put(issuePersonalSort.getProperty(), issuePersonalSort);
        }

        String projectCodePrefix = "pro_";
        String organizationCodePrefix = "org_";
        String customFieldSortPrefix = "foundation.";
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
        } catch (JsonProcessingException e) {
            throw new CommonException("error.gantt.sortJson.serialization", e);
        }
        personalSort.setSortJson(sortJson);
        this.personalSortRepository.updateOptional(personalSort, IssuePersonalSortDTO.FIELD_SORT_JSON);
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
            list = list.stream().sorted(Comparator.comparingInt(e -> Optional.ofNullable(e.getSort()).orElse(Integer.MAX_VALUE))).collect(Collectors.toList());
            layoutVO.setListLayoutColumnRelVOS(modelMapper.map(list, new TypeToken<List<ListLayoutColumnRelVO>>() {}.getType()));
        }
        return layoutVO;
    }
}
