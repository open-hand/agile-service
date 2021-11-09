package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.MoveWorkGroupVO;
import io.choerodon.agile.api.vo.WorkGroupTreeVO;
import io.choerodon.agile.api.vo.WorkGroupVO;
import io.choerodon.agile.app.service.WorkGroupService;
import io.choerodon.agile.app.service.WorkGroupUserRelService;
import io.choerodon.agile.infra.dto.WorkGroupDTO;
import io.choerodon.agile.infra.dto.WorkLogDTO;
import io.choerodon.agile.infra.mapper.WorkGroupMapper;
import io.choerodon.agile.infra.mapper.WorkGroupUserRelMapper;
import io.choerodon.agile.infra.utils.RankUtil;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.collections4.CollectionUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-11-08 15:37
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkGroupServiceImpl implements WorkGroupService {

    @Autowired
    private WorkGroupMapper workGroupMapper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private WorkGroupUserRelMapper workGroupUserRelMapper;

    @Autowired
    private WorkGroupUserRelService workGroupUserRelService;

    @Override
    public WorkGroupTreeVO queryWorkGroupTree(Long organizationId) {
        List<WorkGroupDTO> workGroupDTOS = workGroupMapper.selectByOrganiztionId(organizationId);
        WorkGroupTreeVO workGroupTreeVO = new WorkGroupTreeVO();
        if (CollectionUtils.isEmpty(workGroupDTOS)) {
            return workGroupTreeVO;
        }
        List<WorkGroupVO> workGroupVOList = modelMapper.map(workGroupDTOS, new TypeToken<List<WorkGroupVO>>() {
        }.getType());
        Map<Long, List<WorkGroupVO>> workGroupMap = workGroupVOList.stream().collect(Collectors.groupingBy(WorkGroupVO::getParentId));
        // 构造树形结构
        List<WorkGroupVO> rootWorkGroups = workGroupMap.get(0L);
        if (CollectionUtils.isEmpty(rootWorkGroups)) {
            return workGroupTreeVO;
        }
        workGroupTreeVO.setRootIds(rootWorkGroups.stream().map(WorkGroupVO::getId).collect(Collectors.toList()));
        Map<Long, Set<Long>> workGroupUserMap = workGroupUserRelService.getWorkGroupMap(organizationId);
        handlerChildren(rootWorkGroups, workGroupMap, workGroupUserMap);
        workGroupTreeVO.setWorkGroupVOS(rootWorkGroups);
        return workGroupTreeVO;
    }

    private void handlerChildren(List<WorkGroupVO> rootWorkGroups, Map<Long, List<WorkGroupVO>> workGroupMap, Map<Long, Set<Long>> workGroupUserMap) {
        rootWorkGroups.forEach(v -> {
            List<WorkGroupVO> workGroupVOList = workGroupMap.get(v.getId());
            Set<Long> userIds = workGroupUserMap.getOrDefault(v.getId(), new HashSet<>());
            if (!CollectionUtils.isEmpty(workGroupVOList)) {
                handlerChildren(workGroupVOList, workGroupMap, workGroupUserMap);
                workGroupVOList.forEach(workGroupVO -> userIds.addAll(workGroupVO.getUserIds()));
                v.setChildrens(workGroupVOList);
            }
            v.setUserCount(userIds.size());
            v.setUserIds(userIds);
        });
    }

    @Override
    public WorkGroupVO create(Long organizationId, WorkGroupVO workGroupVO) {
        validate(workGroupVO);
        if (checkName(organizationId, workGroupVO.getParentId(), workGroupVO.getName())) {
            throw new CommonException("error.work.group.name.exist");
        }
        WorkGroupDTO groupDTO = modelMapper.map(workGroupVO, WorkGroupDTO.class);
        // 查询最大的rank值
        String minRank = workGroupMapper.queryMinRank(organizationId, workGroupVO.getParentId());
        groupDTO.setRank(ObjectUtils.isEmpty(minRank) ? RankUtil.mid() : RankUtil.genPre(minRank));
        groupDTO.setOrganizationId(organizationId);
        return modelMapper.map(baseCreate(groupDTO), WorkGroupVO.class);
    }

    private WorkGroupDTO baseCreate(WorkGroupDTO workGroupDTO) {
        if (workGroupMapper.insertSelective(workGroupDTO) != 1) {
            throw new CommonException("error.work.group.insert");
        }
        return workGroupDTO;
    }

    void validate(WorkGroupVO workGroupVO) {
        if (ObjectUtils.isEmpty(workGroupVO.getParentId())) {
            throw new CommonException("error.work.group.parent.null");
        }
        if (ObjectUtils.isEmpty(workGroupVO.getName())) {
            throw new CommonException("error.work.group.name.null");
        }
    }

    @Override
    public WorkGroupVO update(Long organizationId, WorkGroupVO workGroupVO) {
        if (checkName(organizationId, workGroupVO.getParentId(), workGroupVO.getName())) {
            throw new CommonException("error.work.group.name.exist");
        }
        WorkGroupDTO groupDTO = modelMapper.map(workGroupVO, WorkGroupDTO.class);
        baseUpdate(groupDTO);
        return queryById(organizationId, workGroupVO.getId());
    }

    private WorkGroupDTO baseUpdate(WorkGroupDTO workGroupDTO) {
        if (workGroupMapper.updateByPrimaryKeySelective(workGroupDTO) != 1) {
            throw new CommonException("error.work.group.update");
        }
        return workGroupDTO;
    }

    @Override
    public void delete(Long organizationId, Long workGroupId) {
        // 查询当前工作组下面的所有子工作组
        WorkGroupDTO workGroupDTO = new WorkGroupDTO();
        workGroupDTO.setOrganizationId(organizationId);
        Map<Long, List<Long>> map = queryMap(workGroupDTO);
        List<Long> workGroupIds = new ArrayList<>();
        queryChildrenNode(workGroupIds, workGroupId, map);
        if (CollectionUtils.isNotEmpty(workGroupIds)) {
            // 删除工作组下关联的团队成员
            workGroupUserRelMapper.deleteByWorkGroupIds(organizationId, workGroupIds);
            // 删除出工作组
            workGroupMapper.deleteByWorkGroupIds(organizationId, workGroupIds);
        }
    }

    private void queryChildrenNode(List<Long> workGroupIds, Long workGroupId, Map<Long, List<Long>> map) {
        List<Long> list = map.getOrDefault(workGroupId, new ArrayList<>());
        for (Long workGroup : list) {
            queryChildrenNode(workGroupIds, workGroup, map);
        }
        workGroupIds.add(workGroupId);
    }

    @Override
    public WorkGroupVO queryById(Long organizationId, Long workGroupId) {
        WorkGroupDTO workGroupDTO = workGroupMapper.selectByPrimaryKey(workGroupId);
        return modelMapper.map(workGroupDTO, WorkGroupVO.class);
    }

    @Override
    public Boolean checkName(Long organizationId, Long parentId, String name) {
        WorkGroupDTO workGroupDTO = new WorkGroupDTO();
        workGroupDTO.setOrganizationId(organizationId);
        workGroupDTO.setName(name);
        workGroupDTO.setParentId(parentId);
        List<WorkGroupDTO> workGroupDTOS = workGroupMapper.select(workGroupDTO);
        return CollectionUtils.isNotEmpty(workGroupDTOS);
    }

    private Map<Long, List<Long>> queryMap(WorkGroupDTO workGroupDTO) {
        List<WorkGroupDTO> workGroupDTOS = workGroupMapper.select(workGroupDTO);
        if (CollectionUtils.isEmpty(workGroupDTOS)) {
            return new HashMap<>();
        }
        return workGroupDTOS.stream()
                .collect(Collectors.groupingBy(WorkGroupDTO::getParentId, Collectors.mapping(WorkGroupDTO::getId, Collectors.toList())));
    }

    @Override
    public WorkGroupVO moveWorkGroup(Long organizationId, Long parentId, MoveWorkGroupVO moveWorkGroupVO) {
        String rank = null;
        if (Boolean.TRUE.equals(moveWorkGroupVO.getBefore())) {
            rank = getBeforeRank(organizationId, parentId, moveWorkGroupVO);
        } else {
            rank = getAfterRank(organizationId, parentId, moveWorkGroupVO);
        }
        WorkGroupDTO workGroupDTO = workGroupMapper.selectByPrimaryKey(moveWorkGroupVO.getWorkGroupId());
        workGroupDTO.setRank(rank);
        workGroupDTO.setParentId(parentId);
        baseUpdate(workGroupDTO);
        return queryById(organizationId, moveWorkGroupVO.getWorkGroupId());
    }

    @Override
    public List<Long> listChildrenWorkGroup(Long organizationId, Long workGroupId) {
        WorkGroupDTO workGroupDTO = new WorkGroupDTO();
        workGroupDTO.setOrganizationId(organizationId);
        workGroupDTO.setParentId(workGroupId);
        Map<Long, List<Long>> map = queryMap(workGroupDTO);
        List<Long> workGroupIds = new ArrayList<>();
        queryChildrenNode(workGroupIds, workGroupId, map);
        return workGroupIds;
    }

    private String getAfterRank(Long organizationId, Long parentId, MoveWorkGroupVO moveWorkGroupVO) {
        String leftRank = workGroupMapper.queryRank(organizationId, parentId, moveWorkGroupVO.getOutSetId());
        if (ObjectUtils.isEmpty(leftRank)) {
            leftRank = RankUtil.mid();
        }
        String rightRank = workGroupMapper.queryRightRank(organizationId, parentId, leftRank);
        return ObjectUtils.isEmpty(rightRank) ? RankUtil.genNext(leftRank) : RankUtil.between(leftRank, rightRank);
    }

    private String getBeforeRank(Long organizationId, Long parentId, MoveWorkGroupVO moveWorkGroupVO) {
        if (Objects.equals(0L, moveWorkGroupVO.getOutSetId())) {
            String minRank = workGroupMapper.queryMinRank(organizationId, parentId);
            return ObjectUtils.isEmpty(minRank) ? RankUtil.mid() : RankUtil.genPre(minRank);
        } else {
            String rightRank = workGroupMapper.queryRank(organizationId, parentId, moveWorkGroupVO.getOutSetId());
            String leftRank = workGroupMapper.queryLeftRank(organizationId, parentId, rightRank);
            return RankUtil.between(leftRank, rightRank);
        }
    }
}
