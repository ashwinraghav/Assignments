SET SQL DIALECT 3;

CREATE DATABASE 'agent2'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 4096;



/* Domains definitions */

CREATE DOMAIN DBOOLEAN AS SMALLINT
DEFAULT 0
NOT NULL
CHECK ((value = 1) or (value = 0));

CREATE DOMAIN DDATETIME AS TIMESTAMP;

CREATE DOMAIN DMESSAGE AS VARCHAR (1024) CHARACTER SET WIN1251;

CREATE DOMAIN DNAME AS VARCHAR (64) CHARACTER SET WIN1251;

CREATE DOMAIN DREFERENCE AS INTEGER;

CREATE DOMAIN DSTATUS AS CHAR (3) CHARACTER SET WIN1251
NOT NULL
CHECK ((value = 'OK') or (value = 'RUN') or (value = 'ERR'));

CREATE DOMAIN DTEXT AS BLOB sub_type 1 segment size 255 CHARACTER SET WIN1251;



/* Generators definitions */

CREATE GENERATOR GEN_CATEGORY_ID ;
CREATE GENERATOR GEN_HISTORY_ID ;
CREATE GENERATOR GEN_RUN_ID ;
CREATE GENERATOR GEN_SCHEDULE_ID ;
CREATE GENERATOR GEN_STEP_ID ;
CREATE GENERATOR GEN_TASK_ID ;
SET GENERATOR GEN_CATEGORY_ID TO 3;

SET GENERATOR GEN_HISTORY_ID TO 0;

SET GENERATOR GEN_RUN_ID TO 0;

SET GENERATOR GEN_SCHEDULE_ID TO 11;

SET GENERATOR GEN_STEP_ID TO 10;

SET GENERATOR GEN_TASK_ID TO 10;



/* Tables definitions */

CREATE TABLE CATEGORY (
    CATEGORY_ID DREFERENCE NOT NULL,
    NAME DNAME);

CREATE TABLE HISTORY (
    HISTORY_ID DREFERENCE NOT NULL,
    PARENT_ID DREFERENCE,
    TASK_ID DREFERENCE,
    STEP_ID DREFERENCE,
    START_TIME DDATETIME NOT NULL,
    END_TIME DDATETIME,
    STATUS DSTATUS NOT NULL,
    TEXT_MESSAGE DMESSAGE);

CREATE TABLE RUN (
    RUN_ID DREFERENCE NOT NULL,
    SCHEDULE_ID DREFERENCE NOT NULL,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER DEFAULT -1);

CREATE TABLE SCHEDULE (
    SCHEDULE_ID DREFERENCE NOT NULL,
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    DEL_KIND SMALLINT DEFAULT 0 NOT NULL,
    INCREMENT TIMESTAMP NOT NULL,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER);

CREATE TABLE STEP (
    STEP_ID DREFERENCE NOT NULL,
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    ITEM_ORDER SMALLINT NOT NULL,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA DTEXT NOT NULL);

CREATE TABLE TASK (
    TASK_ID DREFERENCE NOT NULL,
    NAME DNAME,
    ENABLED DBOOLEAN NOT NULL,
    CATEGORY_ID DREFERENCE,
    DESCRIPTION DTEXT);



/* Primary keys definition */

ALTER TABLE CATEGORY ADD CONSTRAINT PK_CATEGORY PRIMARY KEY (CATEGORY_ID);
ALTER TABLE HISTORY ADD CONSTRAINT PK_HISTORY PRIMARY KEY (HISTORY_ID);
ALTER TABLE RUN ADD CONSTRAINT PK_RUN PRIMARY KEY (RUN_ID);
ALTER TABLE SCHEDULE ADD CONSTRAINT PK_SCHEDULE PRIMARY KEY (SCHEDULE_ID);
ALTER TABLE STEP ADD CONSTRAINT PK_STEP PRIMARY KEY (STEP_ID);
ALTER TABLE TASK ADD CONSTRAINT PK_TASK PRIMARY KEY (TASK_ID);


/* Foreign keys definition */

ALTER TABLE HISTORY ADD CONSTRAINT FK_HISTORY_ST FOREIGN KEY (STEP_ID) REFERENCES STEP (STEP_ID) ON DELETE CASCADE;
ALTER TABLE HISTORY ADD CONSTRAINT FK_HISTORY_TA FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE;
ALTER TABLE RUN ADD CONSTRAINT FK_RUN_SC FOREIGN KEY (SCHEDULE_ID) REFERENCES SCHEDULE (SCHEDULE_ID) ON DELETE CASCADE;
ALTER TABLE SCHEDULE ADD CONSTRAINT FK_SCHEDULE FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE STEP ADD CONSTRAINT FK_STEP_TASK_ID FOREIGN KEY (TASK_ID) REFERENCES TASK (TASK_ID) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE TASK ADD CONSTRAINT FK_TASK FOREIGN KEY (CATEGORY_ID) REFERENCES CATEGORY (CATEGORY_ID) ON DELETE CASCADE ON UPDATE CASCADE;


/* Indices definition */

CREATE UNIQUE INDEX CATEGORY_IDX ON CATEGORY (NAME);
CREATE INDEX FK_HISTORY_ST ON HISTORY (STEP_ID);
CREATE INDEX FK_HISTORY_TA ON HISTORY (TASK_ID);
CREATE INDEX FK_RUN_RE ON RUN (SCHEDULE_ID);
CREATE INDEX FK_RUN_SC ON RUN (SCHEDULE_ID);
CREATE INDEX FK_SCHEDULE ON SCHEDULE (TASK_ID);
CREATE INDEX FK_STEP_TASK_ID ON STEP (TASK_ID);
CREATE INDEX FK_TASK ON TASK (CATEGORY_ID);
CREATE INDEX IX_RUN_NE ON RUN (NEXT_RUN);
CREATE INDEX IX_STEP_IDOR ON STEP (TASK_ID, ITEM_ORDER);
CREATE INDEX IX_TASK_NA ON TASK (NAME);
CREATE UNIQUE INDEX PK_CATEGORY ON CATEGORY (CATEGORY_ID);
CREATE UNIQUE INDEX PK_HISTORY ON HISTORY (HISTORY_ID);
CREATE UNIQUE INDEX PK_RUN ON RUN (RUN_ID);
CREATE UNIQUE INDEX PK_SCHEDULE ON SCHEDULE (SCHEDULE_ID);
CREATE UNIQUE INDEX PK_STEP ON STEP (STEP_ID);
CREATE UNIQUE INDEX PK_TASK ON TASK (TASK_ID);


SET TERM ^ ; 

/* Stored procedures definition */


/* Stored Procedure: DO_CATEGORY_AI */
CREATE PROCEDURE DO_CATEGORY_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_CATEGORY_CHECK */
CREATE PROCEDURE DO_CATEGORY_CHECK (
    NAME VARCHAR (30))
RETURNS (
    CATEGORY_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_CATEGORY_DEL */
CREATE PROCEDURE DO_CATEGORY_DEL (
    CATEGORY_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_HISTORY_CHECK */
CREATE PROCEDURE DO_HISTORY_CHECK (
    HISTORY_ID INTEGER,
    TASK_ID INTEGER,
    STEP_ID INTEGER,
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_AI */
CREATE PROCEDURE DO_RUN_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_DEL */
CREATE PROCEDURE DO_RUN_DEL (
    RUN_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_INS */
CREATE PROCEDURE DO_RUN_INS (
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
RETURNS (
    RUN_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_RUN_UPD */
CREATE PROCEDURE DO_RUN_UPD (
    RUN_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_SCHEDULE_AI */
CREATE PROCEDURE DO_SCHEDULE_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_SCHEDULE_CHECK */
CREATE PROCEDURE DO_SCHEDULE_CHECK (
    SCHEDULE_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
RETURNS (
    NEW_SCHEDULE_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_SCHEDULE_DEL */
CREATE PROCEDURE DO_SCHEDULE_DEL (
    SCHEDULE_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_AI */
CREATE PROCEDURE DO_STEP_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_CHECK */
CREATE PROCEDURE DO_STEP_CHECK (
    STEP_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_STEP_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_STEP_DEL */
CREATE PROCEDURE DO_STEP_DEL (
    STEP_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_TASK_AI */
CREATE PROCEDURE DO_TASK_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_TASK_CHECK */
CREATE PROCEDURE DO_TASK_CHECK (
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    DESCRIPTION BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_TASK_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: DO_TASK_DEL */
CREATE PROCEDURE DO_TASK_DEL (
    TASK_ID INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_CATEGORY_LIST */
CREATE PROCEDURE QRY_CATEGORY_LIST 
RETURNS (
    CATEGORY_ID INTEGER,
    NAME VARCHAR (30))
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_HISTORY_TASK */
CREATE PROCEDURE QRY_HISTORY_TASK (
    TASK_ID INTEGER,
    SHOW_STEP SMALLINT)
RETURNS (
    NAME VARCHAR (64),
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_RUN */
CREATE PROCEDURE QRY_RUN 
RETURNS (
    RUN_ID INTEGER,
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_SCHEDULE */
CREATE PROCEDURE QRY_SCHEDULE (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_SCHEDULES */
CREATE PROCEDURE QRY_SCHEDULES (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_STEP */
CREATE PROCEDURE QRY_STEP (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_STEPS */
CREATE PROCEDURE QRY_STEPS (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


/* Stored Procedure: QRY_TASK_LIST */
CREATE PROCEDURE QRY_TASK_LIST 
RETURNS (
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    CATEGORY_NAME VARCHAR (30),
    DESCRIPTION BLOB sub_type 1 segment size 255)
AS
BEGIN
  EXIT;
END
^


ALTER PROCEDURE DO_CATEGORY_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  NEW_VALUE = GEN_ID(GEN_CATEGORY_ID, 1);
END
^


ALTER PROCEDURE DO_CATEGORY_CHECK (
    NAME VARCHAR (30))
RETURNS (
    CATEGORY_ID INTEGER)
AS
declare variable uname varchar(30);
begin
  uname = upper(:name);

  select first 1 category_id
  from category
  where name = :uname
  into :category_id;

  if (category_id is null) then
  begin
    execute procedure do_category_ai returning_values :category_id;

    insert into category
    (category_id, name)
    values
    (:category_id, :uname);
  end
end
^


ALTER PROCEDURE DO_CATEGORY_DEL (
    CATEGORY_ID INTEGER)
AS
begin
  delete
  from category
  where category_id = :category_id;
end
^


ALTER PROCEDURE DO_HISTORY_CHECK (
    HISTORY_ID INTEGER,
    TASK_ID INTEGER,
    STEP_ID INTEGER,
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
begin
  if ((not history_id is null) and (history_id <> 0)) then
    update history set
      history_id = :history_id,
      task_id = :task_id,
      step_id = :step_id,
      start_time = :start_time,
      end_time = :end_time,
      status = :status,
      text_message = :text_message
    where
      history_id = :history_id;
  else
    insert into history (
      history_id,
      task_id,
      step_id,
      start_time,
      end_time,
      status,
      text_message)
    values (
      :history_id,
      :task_id,
      :step_id,
      :start_time,
      :end_time,
      :status,
      :text_message);
end
^


ALTER PROCEDURE DO_RUN_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  NEW_VALUE = GEN_ID(GEN_RUN_ID, 1);
END
^


ALTER PROCEDURE DO_RUN_DEL (
    RUN_ID INTEGER)
AS
BEGIN
  DELETE FROM RUN
  WHERE
    (RUN_ID = :RUN_ID);
END
^


ALTER PROCEDURE DO_RUN_INS (
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
RETURNS (
    RUN_ID INTEGER)
AS
BEGIN
  execute procedure do_run_ai returning_values :RUN_ID;

  INSERT INTO RUN (
    RUN_ID,
    SCHEDULE_ID,
    LAST_RUN,
    NEXT_RUN,
    REPEAT)
  VALUES (
    :RUN_ID,
    :SCHEDULE_ID,
    :LAST_RUN,
    :NEXT_RUN,
    :REPEAT);
END
^


ALTER PROCEDURE DO_RUN_UPD (
    RUN_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER)
AS
BEGIN
  if (repeat <> 0) then
  begin
    UPDATE RUN
    SET
      LAST_RUN = :LAST_RUN,
      NEXT_RUN = :NEXT_RUN,
      REPEAT = :REPEAT
    WHERE
      (RUN_ID = :RUN_ID);
  end else
    execute procedure do_run_del(:run_id);
END
^


ALTER PROCEDURE DO_SCHEDULE_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  NEW_VALUE = GEN_ID(GEN_SCHEDULE_ID, 1);
END
^


ALTER PROCEDURE DO_SCHEDULE_CHECK (
    SCHEDULE_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
RETURNS (
    NEW_SCHEDULE_ID INTEGER)
AS
begin
  if (exists (select * from schedule where schedule_id = :schedule_id)) then
  begin
    delete from run
    where schedule_id = :schedule_id;

    new_schedule_id = schedule_id;

    update schedule set
      task_id = :task_id,
      name = :name,
      enabled = :enabled,
      del_kind = :del_kind,
      increment = :increment,
      day_filter_kind = :day_filter_kind,
      day_filter = :day_filter,
      month_filter = :month_filter,
      dow_pos_filter = :dow_pos_filter,
      start_time = :start_time,
      start_date = :start_date,
      end_time = :end_time,
      end_date = :end_date,
      run_count = :run_count
    where
      schedule_id = :schedule_id;
  end else
  begin
    execute procedure do_schedule_ai returning_values :new_schedule_id;

    insert into schedule (
      schedule_id,
      task_id,
      name,
      enabled,
      del_kind,
      increment,
      day_filter_kind,
      day_filter,
      month_filter,
      dow_pos_filter,
      start_time,
      start_date,
      end_time,
      end_date,
      run_count)
    values (
      :new_schedule_id,
      :task_id,
      :name,
      :enabled,
      :del_kind,
      :increment,
      :day_filter_kind,
      :day_filter,
      :month_filter,
      :dow_pos_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count);
  end
end
^


ALTER PROCEDURE DO_SCHEDULE_DEL (
    SCHEDULE_ID INTEGER)
AS
begin
  delete from schedule
  where
  schedule_id = :schedule_id;
end
^


ALTER PROCEDURE DO_STEP_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  NEW_VALUE = GEN_ID(GEN_STEP_ID, 1);
END
^


ALTER PROCEDURE DO_STEP_CHECK (
    STEP_ID INTEGER,
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_STEP_ID INTEGER)
AS
declare variable old_step_id integer;
declare variable old_item_order smallint;
declare variable new_item_order smallint;
begin
  if (exists (select * from step where step_id = :step_id)) then
  begin
    new_step_id = step_id;

    select first 1 step_id, item_order
    from step
    where
    task_id = :task_id and
    item_order = :item_order
    into :old_step_id, :old_item_order;

    if (not old_step_id is null and old_step_id <> step_id) then
    begin
      select first 1 item_order
      from step
      where
      step_id = :step_id
      into :new_item_order;

      update step
      set item_order = :new_item_order
      where step_id = :old_step_id;
    end

    update step
    set
      task_id = :task_id,
      name = :name,
      enabled = :enabled,
      item_order = :item_order,
      on_success = :on_success,
      on_failure = :on_failure,
      data = :data
    where
      step_id = :step_id;
  end else
  begin
    execute procedure do_step_ai returning_values :new_step_id;

    insert into step (
      step_id,
      task_id,
      name,
      enabled,
      item_order,
      on_success,
      on_failure,
      data)
    values (
      :new_step_id,
      :task_id,
      :name,
      :enabled,
      :item_order,
      :on_success,
      :on_failure,
      :data);
  end
end
^


ALTER PROCEDURE DO_STEP_DEL (
    STEP_ID INTEGER)
AS
BEGIN
  DELETE FROM STEP
  WHERE
    (STEP_ID = :STEP_ID);
END
^


ALTER PROCEDURE DO_TASK_AI 
RETURNS (
    NEW_VALUE INTEGER)
AS
BEGIN
  NEW_VALUE = GEN_ID(GEN_TASK_ID, 1);
END
^


ALTER PROCEDURE DO_TASK_CHECK (
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    DESCRIPTION BLOB sub_type 1 segment size 255)
RETURNS (
    NEW_TASK_ID INTEGER)
AS
begin
  if (exists (select * from task where task_id = :task_id)) then
  begin
    new_task_id = task_id;

    update task
    set
      name = :name,
      enabled = :enabled,
      category_id = :category_id,
      description = :description
    where
      task_id = :task_id;
  end else
  begin
    execute procedure do_task_ai returning_values :new_task_id;

    insert into task (
      task_id,
      name,
      enabled,
      category_id,
      description)
    values (
      :new_task_id,
      :name,
      :enabled,
      :category_id,
      :description);
  end
end
^


ALTER PROCEDURE DO_TASK_DEL (
    TASK_ID INTEGER)
AS
BEGIN
  DELETE FROM TASK
  WHERE
    (TASK_ID = :TASK_ID);
END
^


ALTER PROCEDURE QRY_CATEGORY_LIST 
RETURNS (
    CATEGORY_ID INTEGER,
    NAME VARCHAR (30))
AS
BEGIN
  FOR
    select CATEGORY_ID, NAME from category
    order by name

    INTO
      :CATEGORY_ID,
      :NAME
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_HISTORY_TASK (
    TASK_ID INTEGER,
    SHOW_STEP SMALLINT)
RETURNS (
    NAME VARCHAR (64),
    START_TIME TIMESTAMP,
    END_TIME TIMESTAMP,
    STATUS CHAR (3),
    TEXT_MESSAGE VARCHAR (1024))
AS
begin
  if (show_step = 0) then
  begin
    for
      select
      null,
      t.start_time,
      t.end_time,
      t.status,
      t.text_message
      from history t
      where t.task_id = :task_id and t.step_id = 0

      into
        :name,
        :start_time,
        :end_time,
        :status,
        :text_message
    do
      suspend;
  end else
  begin
    for
      select
      s.name,
      t.start_time,
      t.end_time,
      t.status,
      t.text_message
      from history t
      left join step s on s.step_id = t.step_id
      where t.task_id = :task_id
      order by 
      s.item_order 
      nulls first

      into
        :name,
        :start_time,
        :end_time,
        :status,
        :text_message
    do
      suspend;
  end
end
^


ALTER PROCEDURE QRY_RUN 
RETURNS (
    RUN_ID INTEGER,
    SCHEDULE_ID INTEGER,
    LAST_RUN TIMESTAMP,
    NEXT_RUN TIMESTAMP,
    REPEAT INTEGER,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
declare variable right_now timestamp;
begin
  right_now = current_timestamp;

  for
    select
      ru.run_id,
      ru.SCHEDULE_id,
      ru.last_run,
      ru.next_run,
      ru.repeat,
      re.increment,
      re.day_filter_kind,
      re.day_filter,
      re.month_filter,
      re.dow_pos_filter,
      re.start_time,
      re.start_date,
      re.end_time,
      re.end_date,
      re.run_count
    from run ru
    join SCHEDULE re on re.SCHEDULE_id = ru.SCHEDULE_id
    where
      ru.next_run <= :right_now
    order by
      ru.next_run

    into
      :run_id,
      :SCHEDULE_id,
      :last_run,
      :next_run,
      :repeat,
      :increment,
      :day_filter_kind,
      :day_filter,
      :dow_pos_filter,
      :month_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count
  do
  begin
    suspend;
  end
end
^


ALTER PROCEDURE QRY_SCHEDULE (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT)
AS
BEGIN
  FOR
    SELECT
      SCHEDULE_ID,
      NAME,
      ENABLED,
      DEL_KIND
    FROM SCHEDULE
    where
      TASK_ID = :TASK_ID
    order by
      name
    INTO
      :SCHEDULE_ID,
      :NAME,
      :ENABLED,
      :DEL_KIND
  DO
      BEGIN
        SUSPEND;
      END
END
^


ALTER PROCEDURE QRY_SCHEDULES (
    TASK_ID INTEGER)
RETURNS (
    SCHEDULE_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    DEL_KIND SMALLINT,
    INCREMENT TIMESTAMP,
    DAY_FILTER_KIND SMALLINT,
    DAY_FILTER INTEGER,
    MONTH_FILTER INTEGER,
    DOW_POS_FILTER INTEGER,
    START_TIME TIME,
    START_DATE DATE,
    END_TIME TIME,
    END_DATE DATE,
    RUN_COUNT INTEGER)
AS
BEGIN
  FOR
    select
      s.schedule_id,
      s.name,
      s.enabled,
      s.del_kind,
      s.increment,
      s.day_filter_kind,
      s.day_filter,
      s.month_filter,
      s.dow_pos_filter,
      s.start_time,
      s.start_date,
      s.end_time,
      s.end_date,
      s.run_count
    from schedule s
    where
      s.task_id = :task_id
    order by
      s.name

    INTO
      :SCHEDULE_ID,
      :NAME,
      :ENABLED,
      :DEL_KIND,
      :increment,
      :day_filter_kind,
      :day_filter,
      :month_filter,
      :dow_pos_filter,
      :start_time,
      :start_date,
      :end_time,
      :end_date,
      :run_count
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_STEP (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ITEM_ORDER SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  FOR
    SELECT
      STEP_ID,
      NAME,
      ENABLED,
      ITEM_ORDER,
      ON_SUCCESS,
      ON_FAILURE,
      DATA
    FROM STEP
    where
      task_id = :task_id
    order by
      item_order
    INTO
      :STEP_ID,
      :NAME,
      :ENABLED,
      :ITEM_ORDER,
      :ON_SUCCESS,
      :ON_FAILURE,
      :DATA
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_STEPS (
    TASK_ID INTEGER)
RETURNS (
    STEP_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    ON_SUCCESS INTEGER,
    ON_FAILURE INTEGER,
    DATA BLOB sub_type 1 segment size 255)
AS
BEGIN
  FOR
    select
    s.step_id, s.name, s.enabled, s.on_success, s.on_failure, s.data
    from step s
    where s.task_id = :task_id
    order by s.item_order

    INTO
      :STEP_ID,
      :NAME,
      :ENABLED,
      :ON_SUCCESS,
      :ON_FAILURE,
      :DATA
  DO
  BEGIN
    SUSPEND;
  END
END
^


ALTER PROCEDURE QRY_TASK_LIST 
RETURNS (
    TASK_ID INTEGER,
    NAME VARCHAR (30),
    ENABLED SMALLINT,
    CATEGORY_ID INTEGER,
    CATEGORY_NAME VARCHAR (30),
    DESCRIPTION BLOB sub_type 1 segment size 255)
AS
BEGIN
  FOR
    SELECT
      t.TASK_ID,
      t.NAME,
      t.ENABLED,
      t.CATEGORY_ID,
      c.name,
      t.DESCRIPTION
    FROM TASK t
    left join category c on c.category_id = t.category_id
    order by t.name
    INTO
      :TASK_ID,
      :NAME,
      :ENABLED,
      :CATEGORY_ID,
      :category_name,
      :DESCRIPTION
  DO
  BEGIN
    SUSPEND;
  END
END
^


SET TERM ; ^

SET TERM ^ ;

/* Triggers definition */



/* Trigger: BI_RUN */
CREATE TRIGGER BI_RUN FOR RUN ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.RUN_ID IS NULL) THEN
      NEW.RUN_ID = GEN_ID(GEN_RUN_ID, 1);
END
^

/* Trigger: CATEGORY_BI */
CREATE TRIGGER CATEGORY_BI FOR CATEGORY ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.CATEGORY_ID IS NULL) THEN
      NEW.CATEGORY_ID = GEN_ID(GEN_CATEGORY_ID,1);
END
^

/* Trigger: HISTORY_BI */
CREATE TRIGGER HISTORY_BI FOR HISTORY ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.HISTORY_ID IS NULL) THEN
      NEW.HISTORY_ID = GEN_ID(GEN_HISTORY_ID, 1);
END
^

/* Trigger: SCHEDULE_BI */
CREATE TRIGGER SCHEDULE_BI FOR SCHEDULE ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.SCHEDULE_ID IS NULL) THEN
      NEW.SCHEDULE_ID = GEN_ID(GEN_SCHEDULE_ID,1);
END
^

/* Trigger: STEP_BI */
CREATE TRIGGER STEP_BI FOR STEP ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.STEP_ID IS NULL) THEN
      NEW.STEP_ID = GEN_ID(GEN_STEP_ID,1);
END
^

/* Trigger: TASK_BI */
CREATE TRIGGER TASK_BI FOR TASK ACTIVE
BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.TASK_ID IS NULL) THEN
      NEW.TASK_ID = GEN_ID(GEN_TASK_ID,1);
END
^

SET TERM ; ^
