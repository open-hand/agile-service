package io.choerodon.agile.infra.upgrade;


import java.sql.Timestamp;
import java.util.Calendar;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import org.hzero.core.base.BaseConstants;

/**
 * 升级数据修复专用入口
 * @author gaokuo.dai@zknow.com 2022-09-06
 */
@Component
public class UpgradeDataFixCommandLineRunner implements CommandLineRunner {

    @Autowired
    private JdbcTemplate jdbcTemplate;
    @Autowired
    private UpgradeDataFixService dataFixService;

    private final Logger logger = LoggerFactory.getLogger(UpgradeDataFixCommandLineRunner.class);

    @Override
    public void run(String... args) throws Exception {
        try {
            this.fixAgilePersonalFilter2_2();
        } catch (Exception ex) {
            this.logger.error(ex.getMessage(), ex);
        }
    }

    /**
     * 2.2版本-修复个人筛选
     */
    private void fixAgilePersonalFilter2_2() {
        final String changeSetId = "2022-09-06-agile-personal-filter-agile-2.2-001";
        final String author = "gaokuo.dai@zknow.com";
        final String fileName = "script/db/z001_agile_data_fix.groovy";
        if(!this.checkShouldRun(changeSetId, author, fileName)) {
            return;
        }
        this.dataFixService.fixAgilePersonalFilter2_2();
        this.writeChangeLog(
                changeSetId,
                author,
                fileName,
                "execute io.choerodon.agile.infra.upgrade.UpgradeDataFixService.fixAgilePersonalFilter2_2()"
        );
    }


    /**
     * 检查是否可以执行
     * @param changeSetId changeSetId
     * @param author 作者
     * @param fileName 逻辑文件名
     * @return 是否可以执行
     */
    private boolean checkShouldRun(String changeSetId, String author, String fileName) {
        if(StringUtils.isBlank(changeSetId) || StringUtils.isBlank(author)) {
            return false;
        }
        Long count = this.jdbcTemplate.query(
                ("select count(*) as count \n" +
                        "        from databasechangelog\n" +
                        "        where id = ?\n" +
                        "          and author = ?" +
                        "          and filename = ?"),
                (ps) -> {
                    ps.setString(1, changeSetId);
                    ps.setString(2, author);
                    ps.setString(3, fileName);
                },
                (rs) -> {
                    if(!rs.next()) {
                        return null;
                    }
                    return rs.getLong(1);
                }
        );
        return count == null || count == 0L;
    }

    /**
     * 写liquibase执行日志
     * @param changeSetId changeSetId
     * @param author 作者
     * @param fileName 逻辑文件名
     * @param description 描述(可空)
     */
    private void writeChangeLog(
            String changeSetId,
            String author,
            String fileName,
            String description
    ) {
        Assert.hasText(changeSetId, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(author, BaseConstants.ErrorCode.NOT_NULL);
        Assert.hasText(fileName, BaseConstants.ErrorCode.NOT_NULL);

        Integer maxOrderExecuted = this.jdbcTemplate.query(
                "select max(ORDEREXECUTED) as MAX_ORDEREXECUTED FROM databasechangelog",
                (rs) -> {
                    if(!rs.next()) {
                        return null;
                    }
                    return rs.getInt(1);
                }
        );
        if(maxOrderExecuted == null) {
            maxOrderExecuted = 1;
        }
        final Integer orderExecuted = maxOrderExecuted + 1;
        Timestamp executeDateTime = new Timestamp(Calendar.getInstance().getTimeInMillis());
        String execType = "EXECUTED";

        this.jdbcTemplate.update(
                ("insert into databasechangelog(id, author, filename, dateexecuted, orderexecuted, exectype, description)\n" +
                        "        values (?,\n" +
                        "                ?,\n" +
                        "                ?,\n" +
                        "                ?,\n" +
                        "                ?,\n" +
                        "                ?,\n" +
                        "                ?)"),
                (ps) -> {
                    ps.setString(1, changeSetId);
                    ps.setString(2, author);
                    ps.setString(3, fileName);
                    ps.setTimestamp(4, executeDateTime);
                    ps.setInt(5, orderExecuted);
                    ps.setString(6, execType);
                    ps.setString(7, description);
                }
        );
    }

}
