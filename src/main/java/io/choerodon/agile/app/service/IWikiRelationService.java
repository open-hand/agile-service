package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.WikiRelationDTO;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/5.
 * Email: fuqianghuang01@gmail.com
 */
public interface IWikiRelationService {

    void createBase(WikiRelationDTO wikiRelationDTO);

    void deleteBase(WikiRelationDTO wikiRelationDTO);

}
