package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.validator.IssueStatusValidator;
import io.choerodon.agile.api.vo.IssueStatusVO;
import io.choerodon.agile.api.vo.StatusAndIssuesVO;
import io.choerodon.agile.api.vo.StatusMoveVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.api.vo.event.AddStatusWithProject;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.ColumnStatusRelDTO;
import io.choerodon.agile.infra.dto.IssueStatusDTO;
import io.choerodon.agile.infra.dto.StatusAndIssuesDTO;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.ColumnStatusRelMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.core.exception.CommonException;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueStatusServiceImpl implements IssueStatusService {

    private static final String AGILE = "Agile:";
    private static final String PIECHART = AGILE + "PieChart";
    private static final String STATUS = "status";

    @Autowired
    private IssueStatusMapper issueStatusMapper;

    @Autowired
    private ColumnStatusRelMapper columnStatusRelMapper;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private ColumnStatusRelService columnStatusRelService;

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    private DataLogRedisUtil dataLogRedisUtil;

    @Autowired
    private IIssueStatusService iIssueStatusService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public IssueStatusVO create(Long projectId, String applyType, IssueStatusVO issueStatusVO) {
        IssueStatusValidator.checkCreateStatus(projectId, issueStatusVO);
        StatusVO statusVO = new StatusVO();
        statusVO.setType(issueStatusVO.getCategoryCode());
        statusVO.setName(issueStatusVO.getName());
        statusVO = projectConfigService.createStatusForAgile(projectId, applyType, statusVO);
        if (statusVO != null && statusVO.getId() != null) {
            Long statusId = statusVO.getId();
            if (issueStatusMapper.selectByStatusId(projectId, statusId) != null) {
                throw new CommonException("error.status.exist");
            }
            issueStatusVO.setCompleted(false);
            issueStatusVO.setStatusId(statusId);
            IssueStatusDTO issueStatusDTO = modelMapper.map(issueStatusVO, IssueStatusDTO.class);
            return modelMapper.map(insertIssueStatus(issueStatusDTO), IssueStatusVO.class);
        } else {
            throw new CommonException("error.status.create");
        }
    }

    @Override
    public IssueStatusVO createStatusByStateMachine(Long projectId, IssueStatusVO issueStatusVO) {
        IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(projectId, issueStatusVO.getStatusId());
        if (issueStatusDTO == null) {
            issueStatusVO.setCompleted(false);
            issueStatusVO.setEnable(false);
            return modelMapper.map(insertIssueStatus(modelMapper.map(issueStatusVO, IssueStatusDTO.class)), IssueStatusVO.class);
        }
        return modelMapper.map(issueStatusDTO, IssueStatusVO.class);
    }

    public Boolean checkColumnStatusRelExist(Long projectId, Long statusId, Long originColumnId) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setStatusId(statusId);
        columnStatusRelDTO.setColumnId(originColumnId);
        columnStatusRelDTO.setProjectId(projectId);
        ColumnStatusRelDTO rel = columnStatusRelMapper.selectOne(columnStatusRelDTO);
        return rel == null;
    }

    @Override
    public void deleteColumnStatusRel(Long organizationId, Long projectId, Long statusId, Long originColumnId) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setStatusId(statusId);
        columnStatusRelDTO.setColumnId(originColumnId);
        columnStatusRelDTO.setProjectId(projectId);
        columnStatusRelDTO.setOrganizationId(organizationId);
        columnStatusRelService.delete(columnStatusRelDTO);
    }

    public void createColumnStatusRel(Long organizationId, Long projectId, Long statusId, StatusMoveVO statusMoveVO) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setStatusId(statusId);
        columnStatusRelDTO.setProjectId(projectId);
        columnStatusRelDTO.setOrganizationId(organizationId);
        columnStatusRelDTO.setColumnId(statusMoveVO.getColumnId());
        if (columnStatusRelMapper.select(columnStatusRelDTO).isEmpty()) {
            ColumnStatusRelDTO columnStatusRel = new ColumnStatusRelDTO();
            columnStatusRel.setColumnId(statusMoveVO.getColumnId());
            columnStatusRel.setPosition(statusMoveVO.getPosition());
            columnStatusRel.setStatusId(statusId);
            columnStatusRel.setProjectId(projectId);
            columnStatusRel.setOrganizationId(organizationId);
            columnStatusRelService.create(columnStatusRel);
        }
    }

    @Override
    public IssueStatusVO moveStatusToColumn(Long projectId, Long statusId, StatusMoveVO statusMoveVO) {
        // 判断是否在同一列中操作，更新列中position
        Boolean sameRow = statusMoveVO.getColumnId().equals(statusMoveVO.getOriginColumnId());
        deleteColumnStatusRel(0L, projectId, statusId, statusMoveVO.getOriginColumnId());
        updateColumnPosition(0L, projectId, statusId, statusMoveVO, sameRow);
        return modelMapper.map(issueStatusMapper.selectByStatusId(projectId, statusId), IssueStatusVO.class);
    }

    @Override
    public IssueStatusVO moveStatusToUnCorrespond(Long projectId, Long statusId, StatusMoveVO statusMoveVO) {
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setStatusId(statusId);
        columnStatusRelDTO.setColumnId(statusMoveVO.getColumnId());
        columnStatusRelDTO.setProjectId(projectId);
        columnStatusRelService.delete(columnStatusRelDTO);
        return modelMapper.map(issueStatusMapper.selectByStatusId(projectId, statusId), IssueStatusVO.class);
    }

    @Override
    public List<StatusAndIssuesVO> queryUnCorrespondStatus(Long projectId, Long boardId, String applyType) {
        List<StatusVO> statusMapVOList = projectConfigService.queryStatusByProjectId(projectId, null, applyType);
        List<Long> realStatusIds = new ArrayList<>();
        for (StatusVO statusMapVO : statusMapVOList) {
            realStatusIds.add(statusMapVO.getId());
        }
        if (realStatusIds.isEmpty()) {
            return new ArrayList<>();
        }
        List<StatusAndIssuesDTO> statusAndIssuesDTOList = issueStatusMapper.queryUnCorrespondStatus(projectId, boardId, realStatusIds);
        if (statusAndIssuesDTOList != null && !statusAndIssuesDTOList.isEmpty()) {
            List<Long> ids = new ArrayList<>();
            for (StatusAndIssuesDTO statusAndIssuesDTO : statusAndIssuesDTOList) {
                ids.add(statusAndIssuesDTO.getStatusId());
            }
            Map<Long, StatusDTO> map = statusService.batchStatusGet(ids);
            for (StatusAndIssuesDTO statusAndIssuesDTO : statusAndIssuesDTOList) {
                StatusDTO status = map.get(statusAndIssuesDTO.getStatusId());
                statusAndIssuesDTO.setCategoryCode(status.getType());
                statusAndIssuesDTO.setName(status.getName());
            }
        }
        List<StatusAndIssuesVO> statusAndIssuesVOList = new ArrayList<>();
        if (statusAndIssuesDTOList != null) {
            statusAndIssuesVOList = modelMapper.map(statusAndIssuesDTOList, new TypeToken<List<StatusAndIssuesVO>>() {
            }.getType());
        }
        return statusAndIssuesVOList;
    }

    private void checkIssueNumOfStatus(Long projectId, Long statusId) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setStatusId(statusId);
        issueDTO.setProjectId(projectId);
        List<IssueDTO> issueDTOList = issueMapper.select(issueDTO);
        if (issueDTOList != null && !issueDTOList.isEmpty()) {
            throw new CommonException("error.statusHasIssues.delete");
        }
    }

    private void checkStatusExist(Long projectId, Long statusId) {
        IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
        issueStatusDTO.setProjectId(projectId);
        issueStatusDTO.setStatusId(statusId);
        IssueStatusDTO res = issueStatusMapper.selectOne(issueStatusDTO);
        if (res == null) {
            throw new CommonException("error.checkStatusExist.get");
        }
    }

    @Override
    public void deleteStatus(Long projectId, Long statusId, String applyType) {
        checkIssueNumOfStatus(projectId, statusId);
        checkStatusExist(projectId, statusId);
        try {
            projectConfigService.removeStatusForAgile(projectId, statusId, applyType);
        } catch (Exception e) {
            throw new CommonException("error.status.delete");
        }
    }

    @Override
    public List<IssueStatusVO> queryIssueStatusList(Long projectId) {
        IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
        issueStatusDTO.setProjectId(projectId);
        return modelMapper.map(issueStatusMapper.select(issueStatusDTO), new TypeToken<List<IssueStatusVO>>() {
        }.getType());
    }

    @Override
    public IssueStatusVO updateStatus(Long projectId, IssueStatusVO issueStatusVO) {
        IssueStatusValidator.checkUpdateStatus(projectId, issueStatusVO);
        IssueStatusDTO issueStatusDTO = modelMapper.map(issueStatusVO, IssueStatusDTO.class);
        return modelMapper.map(iIssueStatusService.update(issueStatusDTO), IssueStatusVO.class);
    }


    @Override
    public IssueStatusDTO insertIssueStatus(IssueStatusDTO issueStatusDTO) {
        if (issueStatusMapper.insert(issueStatusDTO) != 1) {
            throw new CommonException("error.IssueStatus.insert");
        }
        redisUtil.deleteRedisCache(new String[]{PIECHART + issueStatusDTO.getProjectId() + ':' + STATUS + "*"});
        return modelMapper.map(issueStatusMapper.selectByStatusId(issueStatusDTO.getProjectId(), issueStatusDTO.getStatusId()), IssueStatusDTO.class);
    }

    @Override
    public void delete(IssueStatusDTO issueStatusDTO) {
        if (issueStatusMapper.delete(issueStatusDTO) != 1) {
            throw new CommonException("error.status.delete");
        }
        dataLogRedisUtil.deleteByUpdateIssueStatus(issueStatusDTO);
    }

    @Override
    public void batchCreateStatusByProjectIds(List<AddStatusWithProject> addStatusWithProjects, Long userId) {
        issueStatusMapper.batchCreateStatusByProjectIds(addStatusWithProjects, userId);
    }

    @Override
    public void updateColumnPosition(Long organizationId, Long projectId, Long statusId, StatusMoveVO statusMoveVO, Boolean sameRow) {
        //查询移动到目的列的已有的所有状态
        List<ColumnStatusRelDTO> collect = columnStatusRelMapper.selectStatusRel(organizationId, projectId, statusMoveVO.getColumnId(), statusId);
        // 如果不是移动到同一个列，对原列中的状态重新改变position
        if (Boolean.FALSE.equals(sameRow)) {
            updateOlderColumn(organizationId, projectId, statusId, statusMoveVO);
        }
        // 拥有的所有状态为空，直接接创建一个
        if (CollectionUtils.isEmpty(collect)) {
            createColumnStatusRel(organizationId, projectId, statusId, statusMoveVO);
            return;
        }
        int size = collect.size() + 1;
        // 对指定列进行排序，新增传过来的状态
        cycleUpdate(organizationId, projectId, statusId, size, statusMoveVO, collect);
    }

    private void updateOlderColumn(Long organizationId, Long projectId, Long statusId, StatusMoveVO statusMoveVO) {
        // 查询当前列中的状态，按position升序排列
        List<ColumnStatusRelDTO> columnStatusRelDTOS = columnStatusRelMapper.selectStatusRel(organizationId, projectId, statusMoveVO.getOriginColumnId(), statusId);
        if (!CollectionUtils.isEmpty(columnStatusRelDTOS)) {
            for (int i = 0; i < columnStatusRelDTOS.size(); i++) {
                ColumnStatusRelDTO columnStatusRelDTO = columnStatusRelDTOS.get(i);
                columnStatusRelDTO.setPosition(i);
                baseUpdatePosition(columnStatusRelDTO);
            }
        }
    }

    private void cycleUpdate(Long organizationId, Long projectId, Long statusId, int size, StatusMoveVO statusMoveVO, List<ColumnStatusRelDTO> columnStatusRelDTOS) {
        if (CollectionUtils.isEmpty(columnStatusRelDTOS)) {
            return;
        }
        boolean isInsert = false;
        for (int i = 0; i < size; i++) {
            if (statusMoveVO.getPosition() == i) {
                isInsert = true;
                createColumnStatusRel(organizationId, projectId, statusId, statusMoveVO);
            } else {
                int position = i;
                if (Boolean.TRUE.equals(isInsert)) {
                    position = position - 1;
                }
                ColumnStatusRelDTO columnStatusRelDTO = columnStatusRelDTOS.get(position);
                if (ObjectUtils.isEmpty(columnStatusRelDTO)) {
                    break;
                }
                columnStatusRelDTO.setPosition(i);
                baseUpdatePosition(columnStatusRelDTO);
            }
        }
    }

    @Override
    public void baseUpdatePosition(ColumnStatusRelDTO columnStatusRelDTO) {
        if (columnStatusRelMapper.updatePosition(columnStatusRelDTO) != 1) {
            throw new CommonException("error.update.position");
        }
    }
}
