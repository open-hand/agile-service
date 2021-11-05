package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.validator.IssueValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.validator.IssueComponentValidator;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.agile.infra.dto.ComponentForListDTO;
import io.choerodon.agile.infra.dto.ComponentIssueRelDTO;
import io.choerodon.agile.app.service.ComponentIssueRelService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.mapper.ComponentIssueRelMapper;

import io.choerodon.core.domain.Page;

import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.app.service.IssueComponentService;
import io.choerodon.agile.infra.dto.IssueComponentDTO;
import io.choerodon.agile.infra.mapper.IssueComponentMapper;


import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueComponentServiceImpl implements IssueComponentService {

    private static final String AGILE = "Agile:";
    private static final String PIECHART = AGILE + "PieChart";
    private static final String CUSTOM_CHART = AGILE + "CustomChart";
    private static final String CPMPONENT = "component";

    @Autowired
    private IssueComponentMapper issueComponentMapper;

    @Autowired
    private ComponentIssueRelService componentIssueRelService;

    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;

    @Autowired
    private UserService userService;

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    private IssueValidator issueValidator;


    private static final String MANAGER = "manager";
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public IssueComponentVO create(Long projectId, IssueComponentVO issueComponentVO) {
        if (Boolean.TRUE.equals(checkComponentName(projectId, issueComponentVO.getName()))) {
            throw new CommonException("error.componentName.exist");
        }
        IssueComponentValidator.checkCreateComponent(projectId, issueComponentVO);
        IssueComponentDTO issueComponentDTO = modelMapper.map(issueComponentVO, IssueComponentDTO.class);
        return modelMapper.map(createBase(issueComponentDTO), IssueComponentVO.class);
    }

    private Boolean checkNameUpdate(Long projectId, Long componentId, String componentName) {
        IssueComponentDTO issueComponentDTO = issueComponentMapper.selectByPrimaryKey(componentId);
        if (componentName.equals(issueComponentDTO.getName())) {
            return false;
        }
        IssueComponentDTO check = new IssueComponentDTO();
        check.setProjectId(projectId);
        check.setName(componentName);
        List<IssueComponentDTO> issueComponentDTOList = issueComponentMapper.select(check);
        return issueComponentDTOList != null && !issueComponentDTOList.isEmpty();
    }

    @Override
    public IssueComponentVO update(Long projectId, Long id, IssueComponentVO issueComponentVO, List<String> fieldList) {
        if (Boolean.TRUE.equals(checkNameUpdate(projectId, id, issueComponentVO.getName()))) {
            throw new CommonException("error.componentName.exist");
        }
        IssueComponentValidator.checkUpdateComponent(projectId, issueComponentVO);
        issueComponentVO.setComponentId(id);
        IssueComponentDTO issueComponentDTO = modelMapper.map(issueComponentVO, IssueComponentDTO.class);
        return modelMapper.map(updateOptional(issueComponentDTO, fieldList), IssueComponentVO.class);
    }

    private void unRelateIssueWithComponent(Long projectId, Long id) {
        componentIssueRelService.deleteByComponentId(projectId, id);
    }

    private void reRelateIssueWithComponent(Long projectId, Long id, Long relateComponentId) {
        ComponentIssueRelDTO componentIssueRelDTO = new ComponentIssueRelDTO();
        componentIssueRelDTO.setProjectId(projectId);
        componentIssueRelDTO.setComponentId(id);
        List<ComponentIssueRelDTO> componentIssueRelDTOList = componentIssueRelMapper.select(componentIssueRelDTO);
        unRelateIssueWithComponent(projectId, id);
        for (ComponentIssueRelDTO componentIssue : componentIssueRelDTOList) {
            ComponentIssueRelDTO relate = new ComponentIssueRelDTO();
            relate.setProjectId(projectId);
            relate.setIssueId(componentIssue.getIssueId());
            relate.setComponentId(relateComponentId);
            if (Boolean.TRUE.equals(issueValidator.existComponentIssueRel(relate))) {
                componentIssueRelService.create(relate);
            }
        }
    }

    @Override
    public void delete(Long projectId, Long id, Long relateComponentId) {
//        //默认值校验
        if (relateComponentId == null) {
            unRelateIssueWithComponent(projectId, id);
        } else {
            reRelateIssueWithComponent(projectId, id, relateComponentId);
        }
        deleteBase(id);
    }

    @Override
    public IssueComponentVO queryComponentsById(Long projectId, Long id) {
        IssueComponentDTO issueComponentDTO = new IssueComponentDTO();
        issueComponentDTO.setProjectId(projectId);
        issueComponentDTO.setComponentId(id);
        IssueComponentDTO issueComponent = issueComponentMapper.selectOne(issueComponentDTO);
        if (issueComponent == null) {
            throw new CommonException("error.component.get");
        }
        return modelMapper.map(issueComponent, IssueComponentVO.class);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Page<ComponentForListVO> queryComponentByProjectId(Long projectId, Long componentId, Boolean noIssueTest, SearchVO searchVO, PageRequest pageRequest) {
        //处理用户搜索
        Boolean condition = handleSearchUser(searchVO);
        if (Boolean.TRUE.equals(condition)) {
            Page<ComponentForListDTO> componentForListDTOPage =
                    pagedQueryComponentsByOptions(Arrays.asList(projectId), componentId, noIssueTest, searchVO, pageRequest);
            Page<ComponentForListVO> componentForListVOPageInfo = new Page<>();
            componentForListVOPageInfo.setSize(componentForListDTOPage.getSize());
            componentForListVOPageInfo.setTotalElements(componentForListDTOPage.getTotalElements());
            componentForListVOPageInfo.setNumber(componentForListDTOPage.getNumber());
            componentForListVOPageInfo.setNumberOfElements(componentForListDTOPage.getNumberOfElements());
            componentForListVOPageInfo.setTotalPages(componentForListDTOPage.getTotalPages());
            componentForListVOPageInfo.setContent(modelMapper.map(componentForListDTOPage.getContent(), new TypeToken<List<ComponentForListVO>>(){}.getType()));
            if ((componentForListVOPageInfo.getContent() != null) && !componentForListVOPageInfo.getContent().isEmpty()) {
                List<Long> assigneeIds = componentForListVOPageInfo.getContent().stream().filter(componentForListVO -> componentForListVO.getManagerId() != null
                        && !Objects.equals(componentForListVO.getManagerId(), 0L)).map(ComponentForListVO::getManagerId).distinct().collect(Collectors.toList());
                Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
                componentForListVOPageInfo.getContent().forEach(componentForListVO -> {
                    UserMessageDTO userMessageDTO = usersMap.get(componentForListVO.getManagerId());
                    if (!ObjectUtils.isEmpty(userMessageDTO)) {
                        componentForListVO.setManagerName(userMessageDTO.getName());
                        componentForListVO.setManagerLoginName(userMessageDTO.getLoginName());
                        componentForListVO.setManagerRealName(userMessageDTO.getRealName());
                        componentForListVO.setImageUrl(userMessageDTO.getImageUrl());
                    }
                });
            }
            return componentForListVOPageInfo;
        } else {
            return new Page<>();
        }
    }

    @Override
    public Page<ComponentForListDTO> pagedQueryComponentsByOptions(List<Long> projectIds,
                                                                   Long ignoredComponentId,
                                                                   Boolean noIssueTest,
                                                                   SearchVO searchVO,
                                                                   PageRequest pageRequest) {
        Set<Long> toppedComponentIds = getComponentIdsFromSearchVO(searchVO);
        boolean append = !ObjectUtils.isEmpty(toppedComponentIds) && pageRequest.getPage() == 0;
        Set<Long> ignoredComponentIds = new HashSet<>();
        if (!ObjectUtils.isEmpty(ignoredComponentId)) {
            ignoredComponentIds.add(ignoredComponentId);
        }
        List<ComponentForListDTO> components = new ArrayList<>();
        if (append) {
            components.addAll(issueComponentMapper.queryComponentByOption(projectIds, noIssueTest, toppedComponentIds, null, null, null, null));
        }
        ignoredComponentIds.addAll(toppedComponentIds);
        Page<ComponentForListDTO> componentForListDTOPage =
                PageHelper.doPageAndSort(pageRequest,
                        () -> issueComponentMapper.queryComponentByOption(
                                projectIds,
                                noIssueTest,
                                null,
                                toppedComponentIds,
                                searchVO.getSearchArgs(),
                                searchVO.getAdvancedSearchArgs(),
                                searchVO.getContents()));
        components.addAll(componentForListDTOPage.getContent());
        componentForListDTOPage.setContent(components);
        return componentForListDTOPage;
    }

    private Set<Long> getComponentIdsFromSearchVO(SearchVO searchVO) {
        Set<Long> componentIds = new HashSet<>();
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (!ObjectUtils.isEmpty(otherArgs)) {
            List<String> componentIdStr = (List<String>) otherArgs.get("component");
            if (!ObjectUtils.isEmpty(componentIdStr)) {
                componentIdStr.forEach(str -> componentIds.add(Long.valueOf(str)));
            }
        }
        return componentIds;
    }

    private Boolean handleSearchUser(SearchVO searchVO) {
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(MANAGER) != null) {
            String userName = (String) searchVO.getSearchArgs().get(MANAGER);
            if (userName != null && !"".equals(userName)) {
                List<UserVO> userVOS = userService.listUsersByRealNames(Arrays.asList(userName), false);
                if (userVOS != null && !userVOS.isEmpty()) {
                    searchVO.getAdvancedSearchArgs().put("managerId", userVOS.stream().map(UserVO::getId).collect(Collectors.toList()));
                } else {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public List<IssueVO> queryIssuesByComponentId(Long projectId, Long componentId) {
        return modelMapper.map(issueComponentMapper.queryIssuesByComponentId(projectId, componentId), new TypeToken<List<IssueVO>>(){}.getType());
    }

    @Override
    public List<ComponentForListVO> listByProjectIdForTest(Long projectId, Long componentId, Boolean noIssueTest) {
        List<ComponentForListVO> componentForListDTOList = modelMapper.map(
                issueComponentMapper.queryComponentWithIssueNum(projectId, componentId, noIssueTest), new TypeToken<List<ComponentForListVO>>(){}.getType());
        List<Long> assigneeIds = componentForListDTOList.stream().filter(componentForListVO -> componentForListVO.getManagerId() != null
                && !Objects.equals(componentForListVO.getManagerId(), 0L)).map(ComponentForListVO::getManagerId).distinct().collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
        componentForListDTOList.forEach(componentForListVO -> {
            String assigneeName = usersMap.get(componentForListVO.getManagerId()) != null ? usersMap.get(componentForListVO.getManagerId()).getName() : null;
            String imageUrl = assigneeName != null ? usersMap.get(componentForListVO.getManagerId()).getImageUrl() : null;
            componentForListVO.setManagerName(assigneeName);
            componentForListVO.setImageUrl(imageUrl);
        });
        return componentForListDTOList;
    }

    @Override
    public Boolean checkComponentName(Long projectId, String componentName) {
        IssueComponentDTO issueComponentDTO = new IssueComponentDTO();
        issueComponentDTO.setProjectId(projectId);
        issueComponentDTO.setName(componentName);
        List<IssueComponentDTO> issueComponentDTOList = issueComponentMapper.select(issueComponentDTO);
        return issueComponentDTOList != null && !issueComponentDTOList.isEmpty();
    }

    @Override
    public IssueComponentDTO createBase(IssueComponentDTO issueComponentDTO) {
        if (issueComponentMapper.insert(issueComponentDTO) != 1) {
            throw new CommonException("error.scrum_issue_component.insert");
        }
        redisUtil.deleteRedisCache(new String[]{PIECHART + issueComponentDTO.getProjectId() + ':' + CPMPONENT + "*"});
        return modelMapper.map(issueComponentMapper.selectByPrimaryKey(issueComponentDTO.getComponentId()), IssueComponentDTO.class);
    }

    private IssueComponentDTO updateOptional(IssueComponentDTO issueComponentDTO, List<String> fieldList){
        if (issueComponentMapper.updateOptional(issueComponentDTO,fieldList.toArray(new String[fieldList.size()])) != 1) {
            throw new CommonException("error.scrum_issue_component.update");
        }
        redisUtil.deleteRedisCache(new String[]{PIECHART + issueComponentDTO.getProjectId() + ':' + CPMPONENT + "*"});
        return modelMapper.map(issueComponentMapper.selectByPrimaryKey(issueComponentDTO.getComponentId()), IssueComponentDTO.class);
    }

    @Override
    public IssueComponentDTO updateBase(IssueComponentDTO issueComponentDTO) {
        if (issueComponentMapper.updateByPrimaryKeySelective(issueComponentDTO) != 1) {
            throw new CommonException("error.scrum_issue_component.update");
        }
        redisUtil.deleteRedisCache(new String[]{
                PIECHART + issueComponentDTO.getProjectId() + ':' + CPMPONENT + "*",
                CUSTOM_CHART + issueComponentDTO.getProjectId() + ":" + "*"
        });
        return modelMapper.map(issueComponentMapper.selectByPrimaryKey(issueComponentDTO.getComponentId()), IssueComponentDTO.class);
    }

    @Override
    public void deleteBase(Long id) {
        IssueComponentDTO issueComponentDTO = issueComponentMapper.selectByPrimaryKey(id);
        if (issueComponentDTO == null) {
            throw new CommonException("error.component.get");
        }
        if (issueComponentMapper.delete(issueComponentDTO) != 1) {
            throw new CommonException("error.component.delete");
        }
        redisUtil.deleteRedisCache(new String[]{PIECHART + issueComponentDTO.getProjectId() + ':' + CPMPONENT + "*"});
    }
}
