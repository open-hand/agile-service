package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.IWikiRelationService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.WikiRelationDTO;
import io.choerodon.agile.infra.mapper.WikiRelationMapper;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/5.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class IWikiRelationServiceImpl implements IWikiRelationService {

    @Autowired
    private WikiRelationMapper wikiRelationMapper;

    @Override
    @DataLog(type = "knowledgeRelationCreate")
    public void createBase(WikiRelationDTO wikiRelationDTO) {
        if (wikiRelationMapper.insert(wikiRelationDTO) != 1) {
            throw new CommonException("error.wikiRelationDTO.insert");
        }
    }

    @Override
    @DataLog(type = "knowledgeRelationDelete")
    public void deleteBase(WikiRelationDTO wikiRelationDTO) {
        if (wikiRelationMapper.delete(wikiRelationDTO) != 1) {
            throw new CommonException("error.wikiRelationDTO.delete");
        }
    }
}
